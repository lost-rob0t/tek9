(in-package :star-git)

(defparameter +by-couch-rev-index+ "star-git/by-couch-rev")
(defparameter +commit-v2-marker+ "star-git-commit-v2")

(defparameter +commit-v2-provenance-fields+
  '(:couch-rev
    :source-actor
    :operation-id
    :trace-id
    :refresh-generation
    :policy-digest
    :content-digest
    :schema-release
    :schema-version))

(define-condition invalid-provenance (error)
  ((reason :initarg :reason :reader invalid-provenance-reason)
   (key :initarg :key :initform nil :reader invalid-provenance-key))
  (:report
   (lambda (condition stream)
     (format stream "Invalid star-git commit provenance~@[ for ~S~]: ~A"
             (invalid-provenance-key condition)
             (invalid-provenance-reason condition)))))

(defvar *commit-document-v1-function*
  (symbol-function 'commit-document)
  "Original commit-v1 writer retained for backward-compatible FORMAT-VERSION 1.")

(defvar *commit-record-from-preimage-v1-function*
  (symbol-function '%commit-record-from-preimage)
  "Original commit-v1 pack parser retained for mixed-version pack import.")

(defun %provenance-error (reason &optional key)
  (error 'invalid-provenance :reason reason :key key))

(defun %plist-pairs (plist)
  (unless (listp plist)
    (%provenance-error "provenance must be a property list"))
  (when (oddp (length plist))
    (%provenance-error "provenance property list has an odd number of elements"))
  (loop for (key value) on plist by #'cddr
        collect (cons key value)))

(defun %normalize-provenance-string (value key)
  (cond
    ((null value) nil)
    ((stringp value) value)
    (t (%provenance-error "value must be a string or NIL" key))))

(defun %normalize-refresh-generation (value)
  (cond
    ((null value) nil)
    ((and (integerp value) (not (minusp value))) value)
    (t (%provenance-error
        "refresh generation must be a non-negative integer or NIL"
        :refresh-generation))))

(defun normalize-commit-provenance (provenance)
  "Return a closed, fixed-order commit-v2 provenance plist.

Unknown or duplicate keys are rejected so caller plist ordering cannot affect
object identity and secret/transport fields cannot silently enter commit data."
  (let ((seen (make-hash-table :test #'eq))
        (values (make-hash-table :test #'eq)))
    (dolist (pair (%plist-pairs (or provenance nil)))
      (let ((key (car pair))
            (value (cdr pair)))
        (unless (member key +commit-v2-provenance-fields+ :test #'eq)
          (%provenance-error "unknown provenance key" key))
        (when (gethash key seen)
          (%provenance-error "duplicate provenance key" key))
        (setf (gethash key seen) t
              (gethash key values) value)))
    (loop for key in +commit-v2-provenance-fields+
          append
          (list key
                (if (eq key :refresh-generation)
                    (%normalize-refresh-generation (gethash key values))
                    (%normalize-provenance-string (gethash key values) key))))))

(defun commit-format-version (record)
  "Return RECORD's star-git commit format version. Pre-v2 records are v1."
  (or (getf record :format-version) 1))

(defun commit-provenance (record)
  "Return RECORD's normalized commit-v2 provenance, or NIL for commit-v1."
  (and (= (commit-format-version record) 2)
       (copy-list (getf record :provenance))))

(defun %provenance-value-string (key value)
  (if (eq key :refresh-generation)
      (if value
          (write-to-string value :base 10 :radix nil)
          "")
      (or value "")))

(defun %commit-v2-preimage (logical-id blob-id parents tenant dataset dtype
                            mutation-kind accepted-at provenance)
  (babel:string-to-octets
   (with-output-to-string (stream)
     (write-line +commit-v2-marker+ stream)
     (%write-field stream "logical-id" logical-id)
     (%write-field stream "blob-id" blob-id)
     (%write-field stream "tenant" tenant)
     (%write-field stream "dataset" dataset)
     (%write-field stream "dtype" dtype)
     (%write-field stream "mutation" mutation-kind)
     (%write-field stream "accepted-at"
                   (write-to-string accepted-at :base 10 :radix nil))
     (format stream "parents ~D~%" (length parents))
     (dolist (parent parents)
       (%write-field stream "parent" parent))
     (format stream "provenance ~D~%" (length +commit-v2-provenance-fields+))
     (dolist (key +commit-v2-provenance-fields+)
       (%write-field stream
                     (string-downcase (symbol-name key))
                     (%provenance-value-string key (getf provenance key)))))
   :encoding :utf-8))

(defun %commit-v2-record (commit-id logical-id blob-id parents tenant dataset
                          dtype mutation-kind accepted-at provenance)
  (list :format-version 2
        :commit-id commit-id
        :logical-id logical-id
        :blob-id blob-id
        :parents (copy-list parents)
        :tenant (%normalize-string tenant)
        :dataset (%normalize-string dataset)
        :dtype (%normalize-string dtype)
        :mutation-kind (%normalize-string mutation-kind)
        :accepted-at accepted-at
        :provenance (copy-list provenance)))

(defun %couch-rev-index-key (tenant dataset logical-id couch-rev)
  (format nil "~A|~A|~A|~A"
          (%component tenant)
          (%component dataset)
          (%component logical-id)
          (%component couch-rev)))

(defun %commit-couch-rev-index-value (document)
  (let* ((record (doc-value document))
         (provenance (getf record :provenance))
         (couch-rev (and provenance (getf provenance :couch-rev))))
    (and couch-rev
         (plusp (length couch-rev))
         (%couch-rev-index-key (getf record :tenant)
                               (getf record :dataset)
                               (getf record :logical-id)
                               couch-rev))))

(defun %ensure-v2-index (repository)
  "Ensure the process-local Couch-revision index handle is registered."
  (let ((database (repository-database repository)))
    (or (tek9:secondary-index-by-name database +by-couch-rev-index+)
        (register-index database
                        +by-couch-rev-index+
                        #'%commit-couch-rev-index-value
                        :database-name +commits-db+
                        :unique t))))

(defun find-commit-by-couch-rev (repository logical-id couch-rev
                                 &key tenant dataset)
  "Return the unique commit record for scoped Couch revision, or NIL."
  (check-type logical-id string)
  (check-type couch-rev string)
  (%ensure-v2-index repository)
  (let ((matches
          (index-fetch (repository-database repository)
                       +by-couch-rev-index+
                       (%couch-rev-index-key tenant dataset logical-id couch-rev))))
    (cond
      ((null matches) nil)
      ((null (rest matches)) (doc-value (first matches)))
      (t (error 'object-integrity-error
                :id (format nil "couch-rev:~A" couch-rev))))))

(defun %v1-commit-call-arguments (tenant dataset dtype mutation-kind accepted-at
                                  update-ref-p parents parents-supplied-p
                                  expected-head expected-head-supplied-p)
  (let ((arguments
          (list :tenant tenant
                :dataset dataset
                :dtype dtype
                :mutation-kind mutation-kind
                :accepted-at accepted-at
                :update-ref-p update-ref-p)))
    (when parents-supplied-p
      (setf arguments (append arguments (list :parents parents))))
    (when expected-head-supplied-p
      (setf arguments
            (append arguments (list :expected-head expected-head))))
    arguments))

(defun commit-document (repository logical-id blob-id
                        &key
                          tenant
                          dataset
                          dtype
                          (mutation-kind "update")
                          (accepted-at (get-universal-time))
                          (parents nil parents-supplied-p)
                          (expected-head nil expected-head-supplied-p)
                          (update-ref-p t)
                          (format-version 1)
                          provenance)
  "Append one immutable document revision in commit format v1 or v2.

FORMAT-VERSION 1 preserves the original star-git contract and rejects provenance.
FORMAT-VERSION 2 binds a closed normalized provenance block into object identity."
  (ecase format-version
    (1
     (when provenance
       (%provenance-error "commit-v1 cannot encode provenance"))
     (apply *commit-document-v1-function*
            repository logical-id blob-id
            (%v1-commit-call-arguments
             tenant dataset dtype mutation-kind accepted-at update-ref-p
             parents parents-supplied-p expected-head expected-head-supplied-p)))
    (2
     (check-type logical-id string)
     (check-type blob-id string)
     (check-type accepted-at integer)
     (let ((blob (read-object repository blob-id))
           (provenance (normalize-commit-provenance provenance)))
       (unless blob
         (error 'missing-object :id blob-id))
       (unless (string= (getf blob :type) "blob")
         (error 'object-integrity-error :id blob-id))
       (%ensure-v2-index repository)
       (let* ((database (repository-database repository))
              (ref-name
                (document-ref-name logical-id :tenant tenant :dataset dataset)))
         (with-write-transaction (database)
           (let* ((current (%resolve-ref-in-transaction repository ref-name))
                  (parents
                    (if parents-supplied-p
                        (copy-list parents)
                        (if current (list current) nil))))
             (when (and expected-head-supplied-p
                        (not (equal expected-head current)))
               (error 'ref-conflict
                      :ref ref-name
                      :expected expected-head
                      :current current))
             (when (and update-ref-p
                        current
                        parents-supplied-p
                        (not (equal current (first parents))))
               (error 'ref-conflict
                      :ref ref-name
                      :expected current
                      :current (first parents)))
             (dolist (parent parents)
               (%require-commit repository parent))
             (let* ((preimage
                      (%commit-v2-preimage logical-id
                                           blob-id
                                           parents
                                           tenant
                                           dataset
                                           dtype
                                           mutation-kind
                                           accepted-at
                                           provenance))
                    (commit-id (%typed-object-id "commit" preimage))
                    (record
                      (%commit-v2-record commit-id
                                         logical-id
                                         blob-id
                                         parents
                                         tenant
                                         dataset
                                         dtype
                                         mutation-kind
                                         accepted-at
                                         provenance))
                    (existing (read-commit repository commit-id)))
               (%write-object repository "commit" preimage)
               (cond
                 ((null existing)
                  (put* database record
                        :id commit-id
                        :database-name +commits-db+))
                 ((not (equal existing record))
                  (error 'object-integrity-error :id commit-id)))
               (when update-ref-p
                 (lmdb:put (%refs-db repository) ref-name commit-id))
               commit-id))))))))

(defun %parse-v2-integer (string object-id field &key nullable non-negative)
  (when (and nullable (zerop (length string)))
    (return-from %parse-v2-integer nil))
  (let ((value
          (handler-case
              (parse-integer string :junk-allowed nil)
            (error ()
              (%pack-fail object-id "invalid integer field ~A: ~S" field string)))))
    (when (and non-negative (minusp value))
      (%pack-fail object-id "negative integer field ~A: ~D" field value))
    value))

(defun %commit-v2-record-from-preimage (commit-id octets pathname)
  (multiple-value-bind (version cursor)
      (%octet-line octets 0 pathname)
    (unless (string= version +commit-v2-marker+)
      (%pack-fail pathname "unsupported commit-v2 marker ~S" version))
    (labels ((field (name)
               (multiple-value-bind (value next-cursor)
                   (%commit-field octets cursor name pathname)
                 (setf cursor next-cursor)
                 value))
             (line ()
               (multiple-value-bind (value next-cursor)
                   (%octet-line octets cursor pathname)
                 (setf cursor next-cursor)
                 value)))
      (let* ((logical-id (field "logical-id"))
             (blob-id (field "blob-id"))
             (tenant (field "tenant"))
             (dataset (field "dataset"))
             (dtype (field "dtype"))
             (mutation (field "mutation"))
             (accepted-at
               (%parse-v2-integer (field "accepted-at")
                                  pathname
                                  "accepted-at"))
             (parents-line (line)))
        (unless (and (>= (length parents-line) 8)
                     (string= "parents " parents-line :end2 8))
          (%pack-fail pathname "invalid parents header ~S" parents-line))
        (let ((parent-count
                (%parse-v2-integer (subseq parents-line 8)
                                   pathname
                                   "parents"
                                   :non-negative t))
              (parents nil))
          (dotimes (index parent-count)
            (declare (ignore index))
            (push (field "parent") parents))
          (let ((provenance-line (line)))
            (unless (and (>= (length provenance-line) 11)
                         (string= "provenance " provenance-line :end2 11))
              (%pack-fail pathname
                          "invalid provenance header ~S"
                          provenance-line))
            (let ((provenance-count
                    (%parse-v2-integer (subseq provenance-line 11)
                                       pathname
                                       "provenance"
                                       :non-negative t)))
              (unless (= provenance-count
                         (length +commit-v2-provenance-fields+))
                (%pack-fail pathname
                            "unsupported provenance field count ~D"
                            provenance-count)))
            (let* ((raw-provenance
                     (loop for key in +commit-v2-provenance-fields+
                           for wire-name = (string-downcase (symbol-name key))
                           for raw = (field wire-name)
                           append
                           (list key
                                 (cond
                                   ((eq key :refresh-generation)
                                    (%parse-v2-integer raw
                                                       pathname
                                                       wire-name
                                                       :nullable t
                                                       :non-negative t))
                                   ((zerop (length raw)) nil)
                                   (t raw)))))
                   (provenance
                     (normalize-commit-provenance raw-provenance)))
              (unless (= cursor (length octets))
                (%pack-fail pathname
                            "trailing bytes in commit-v2 preimage"))
              (%commit-v2-record commit-id
                                 logical-id
                                 blob-id
                                 (nreverse parents)
                                 tenant
                                 dataset
                                 dtype
                                 mutation
                                 accepted-at
                                 provenance))))))))

(defun %commit-record-from-preimage (commit-id octets pathname)
  "Parse either immutable commit-v1 or commit-v2 bytes during pack import."
  (multiple-value-bind (version ignored-cursor)
      (%octet-line octets 0 pathname)
    (declare (ignore ignored-cursor))
    (cond
      ((string= version "star-git-commit-v1")
       (funcall *commit-record-from-preimage-v1-function*
                commit-id octets pathname))
      ((string= version +commit-v2-marker+)
       (%commit-v2-record-from-preimage commit-id octets pathname))
      (t
       (%pack-fail pathname "unsupported commit preimage ~S" version)))))

(defun %install-imported-commit (repository commit-id preimage pathname)
  "Install a v1/v2 commit record and maintain the v2 Couch revision index."
  (let* ((database (repository-database repository))
         (record (%commit-record-from-preimage commit-id preimage pathname)))
    (when (= (commit-format-version record) 2)
      (%ensure-v2-index repository))
    (let ((existing (read-commit repository commit-id)))
      (cond
        ((null existing)
         (put* database record :id commit-id :database-name +commits-db+))
        ((not (equal existing record))
         (%pack-integrity-fail pathname commit-id
                               "commit metadata disagrees with existing index"))))
    record))
