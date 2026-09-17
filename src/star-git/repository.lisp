(in-package :star-git)

(defconstant +objects-db+ "star-git/objects")
(defconstant +commits-db+ "star-git/commits")
(defconstant +refs-db+ "star-git/refs")

(defconstant +by-document-index+ "star-git/by-document")
(defconstant +by-dataset-day-index+ "star-git/by-dataset-day")
(defconstant +by-content-index+ "star-git/by-content-hash")

(defclass repository ()
  ((database :initarg :database :reader repository-database)
   (owns-database-p :initarg :owns-database-p :initform t)))

(define-condition ref-conflict (error)
  ((ref :initarg :ref :reader ref-conflict-ref)
   (expected :initarg :expected :reader ref-conflict-expected)
   (current :initarg :current :reader ref-conflict-current))
  (:report
   (lambda (condition stream)
     (format stream "star-git ref ~S expected ~S but current head is ~S."
             (ref-conflict-ref condition)
             (ref-conflict-expected condition)
             (ref-conflict-current condition)))))

(define-condition missing-object (error)
  ((id :initarg :id :reader missing-object-id))
  (:report
   (lambda (condition stream)
     (format stream "star-git object ~S does not exist."
             (missing-object-id condition)))))

(define-condition object-integrity-error (error)
  ((id :initarg :id :reader object-integrity-error-id))
  (:report
   (lambda (condition stream)
     (format stream "star-git immutable object ~S does not match its content id."
             (object-integrity-error-id condition)))))

(defun %octet-vector-p (value)
  (and (vectorp value)
       (subtypep (array-element-type value) '(unsigned-byte 8))))

(defun %ensure-octets (value)
  (unless (%octet-vector-p value)
    (error 'type-error
           :datum value
           :expected-type '(vector (unsigned-byte 8))))
  value)

(defun %sha256 (octets)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256 octets)))

(defun %typed-object-id (type octets)
  (let* ((octets (%ensure-octets octets))
         (header (babel:string-to-octets
                  (format nil "star-git ~A ~D~C" type (length octets) #\Null)
                  :encoding :utf-8))
         (preimage (concatenate '(vector (unsigned-byte 8)) header octets)))
    (format nil "sha256:~A" (%sha256 preimage))))

(defun %normalize-string (value)
  (etypecase value
    (null "")
    (string value)
    (symbol (string-downcase (symbol-name value)))))

(defun %day-string (universal-time)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))

(defun %component (value)
  (let ((value (%normalize-string value)))
    (format nil "~D:~A" (length value) value)))

(defun document-ref-name (logical-id &key tenant dataset)
  "Return the deterministic mutable ref name for one logical StarIntel document."
  (format nil "refs/docs/~A/~A/~A"
          (%component tenant)
          (%component dataset)
          (%component logical-id)))

(defun %dataset-day-key (tenant dataset day dtype)
  (format nil "~A|~A|~A|~A"
          (%component tenant)
          (%component dataset)
          day
          (%component dtype)))

(defun %refs-db (repository)
  (database-db (repository-database repository)
               +refs-db+
               :key-encoding :utf-8
               :value-encoding :utf-8))

(defun %object-value (type octets)
  (list :type type :bytes (copy-seq octets)))

(defun %object-matches-p (value type octets)
  (and value
       (string= (getf value :type) type)
       (equalp (getf value :bytes) octets)))

(defun %register-indexes (database)
  (register-index
   database
   +by-document-index+
   (lambda (document)
     (getf (doc-value document) :logical-id))
   :database-name +commits-db+)
  (register-index
   database
   +by-dataset-day-index+
   (lambda (document)
     (let ((value (doc-value document)))
       (%dataset-day-key (getf value :tenant)
                         (getf value :dataset)
                         (%day-string (getf value :accepted-at))
                         (getf value :dtype))))
   :database-name +commits-db+)
  (register-index
   database
   +by-content-index+
   (lambda (document)
     (getf (doc-value document) :blob-id))
   :database-name +commits-db+)
  database)

(defun open-repository (path &key (name "star-git") max-size)
  "Open a star-git repository backed by one Tek9 LMDB environment.

The initial core keeps immutable objects hot in Tek9. LZMA archive packs are a
separate cold-pack layer so compression/repacking never changes object ids."
  (let* ((database
           (if max-size
               (new-database name :path path :max-size max-size)
               (new-database name :path path)))
         (database (open-database database)))
    ;; Fix the DBI encodings before any explicit transaction begins.
    (database-db database +objects-db+
                 :key-encoding :utf-8
                 :value-encoding :octets)
    (database-db database +commits-db+
                 :key-encoding :utf-8
                 :value-encoding :octets)
    (database-db database +refs-db+
                 :key-encoding :utf-8
                 :value-encoding :utf-8)
    (%register-indexes database)
    (make-instance 'repository
                   :database database
                   :owns-database-p t)))

(defun close-repository (repository)
  (when (slot-value repository 'owns-database-p)
    (close-database (repository-database repository)))
  repository)

(defun %write-object (repository type octets)
  (let* ((octets (%ensure-octets octets))
         (id (%typed-object-id type octets))
         (database (repository-database repository)))
    (with-write-transaction (database)
      (let ((existing (fetch* database id :database-name +objects-db+)))
        (cond
          ((null existing)
           (put* database
                 (%object-value type octets)
                 :id id
                 :database-name +objects-db+))
          ((not (%object-matches-p existing type octets))
           (error 'object-integrity-error :id id)))))
    id))

(defun write-blob (repository canonical-octets)
  "Store canonical StarIntel bytes as an immutable content-addressed blob."
  (%write-object repository "blob" canonical-octets))

(defun read-object (repository object-id)
  "Return the immutable object plist for OBJECT-ID, or NIL when absent."
  (fetch* (repository-database repository)
          object-id
          :database-name +objects-db+))

(defun read-blob (repository blob-id)
  "Return canonical bytes for BLOB-ID and reject non-blob object ids."
  (let ((object (read-object repository blob-id)))
    (unless object
      (error 'missing-object :id blob-id))
    (unless (string= (getf object :type) "blob")
      (error 'object-integrity-error :id blob-id))
    (copy-seq (getf object :bytes))))

(defun %write-field (stream name value)
  (let* ((string (%normalize-string value))
         (octets (babel:string-to-octets string :encoding :utf-8)))
    (format stream "~A ~D:" name (length octets))
    (write-string string stream)
    (terpri stream)))

(defun %commit-preimage (logical-id blob-id parents tenant dataset dtype mutation-kind accepted-at)
  (babel:string-to-octets
   (with-output-to-string (stream)
     (write-line "star-git-commit-v1" stream)
     (%write-field stream "logical-id" logical-id)
     (%write-field stream "blob-id" blob-id)
     (%write-field stream "tenant" tenant)
     (%write-field stream "dataset" dataset)
     (%write-field stream "dtype" dtype)
     (%write-field stream "mutation" mutation-kind)
     (%write-field stream "accepted-at" (write-to-string accepted-at :base 10 :radix nil))
     (format stream "parents ~D~%" (length parents))
     (dolist (parent parents)
       (%write-field stream "parent" parent)))
   :encoding :utf-8))

(defun %commit-record (commit-id logical-id blob-id parents tenant dataset dtype mutation-kind accepted-at)
  (list :commit-id commit-id
        :logical-id logical-id
        :blob-id blob-id
        :parents (copy-list parents)
        :tenant (%normalize-string tenant)
        :dataset (%normalize-string dataset)
        :dtype (%normalize-string dtype)
        :mutation-kind (%normalize-string mutation-kind)
        :accepted-at accepted-at))

(defun read-commit (repository commit-id)
  "Return the indexed commit record for COMMIT-ID, or NIL when absent."
  (fetch* (repository-database repository)
          commit-id
          :database-name +commits-db+))

(defun %resolve-ref-in-transaction (repository ref-name)
  (lmdb:g3t (%refs-db repository) ref-name))

(defun resolve-ref (repository ref-name)
  "Return REF-NAME's current commit id, or NIL."
  (let ((database (repository-database repository)))
    (with-read-transaction (database)
      (%resolve-ref-in-transaction repository ref-name))))

(defun %require-commit (repository commit-id)
  (unless (read-commit repository commit-id)
    (error 'missing-object :id commit-id))
  commit-id)

(defun update-ref (repository ref-name new-commit &key (expected :any))
  "Compare-and-swap REF-NAME to NEW-COMMIT.

EXPECTED defaults to :ANY. Pass NIL to require a missing ref or a commit id to
require an exact current head."
  (%require-commit repository new-commit)
  (let ((database (repository-database repository)))
    (with-write-transaction (database)
      (let ((current (%resolve-ref-in-transaction repository ref-name)))
        (unless (or (eq expected :any)
                    (equal expected current))
          (error 'ref-conflict
                 :ref ref-name
                 :expected expected
                 :current current))
        (lmdb:put (%refs-db repository) ref-name new-commit)
        new-commit))))

(defun commit-document (repository logical-id blob-id
                        &key
                          tenant
                          dataset
                          dtype
                          (mutation-kind "update")
                          (accepted-at (get-universal-time))
                          (parents nil parents-supplied-p)
                          (expected-head nil expected-head-supplied-p)
                          (update-ref-p t))
  "Append one immutable logical-document revision and optionally advance its ref.

When PARENTS is omitted, the current document ref becomes the sole parent. When
the ref already exists and explicit PARENTS are supplied, its current head must
remain the first parent so normal writes cannot silently sever history.
EXPECTED-HEAD provides an explicit stale-writer CAS when the caller has one."
  (check-type logical-id string)
  (check-type blob-id string)
  (check-type accepted-at integer)
  (let ((blob (read-object repository blob-id)))
    (unless blob
      (error 'missing-object :id blob-id))
    (unless (string= (getf blob :type) "blob")
      (error 'object-integrity-error :id blob-id)))
  (let* ((database (repository-database repository))
         (ref-name (document-ref-name logical-id :tenant tenant :dataset dataset)))
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
        (let* ((preimage (%commit-preimage logical-id
                                           blob-id
                                           parents
                                           tenant
                                           dataset
                                           dtype
                                           mutation-kind
                                           accepted-at))
               (commit-id (%typed-object-id "commit" preimage))
               (record (%commit-record commit-id
                                       logical-id
                                       blob-id
                                       parents
                                       tenant
                                       dataset
                                       dtype
                                       mutation-kind
                                       accepted-at))
               (existing (read-commit repository commit-id)))
          (%write-object repository "commit" preimage)
          (cond
            ((null existing)
             (put* database record :id commit-id :database-name +commits-db+))
            ((not (equal existing record))
             (error 'object-integrity-error :id commit-id)))
          (when update-ref-p
            (lmdb:put (%refs-db repository) ref-name commit-id))
          commit-id)))))

(defun %commit< (left right)
  (let ((left-time (getf left :accepted-at))
        (right-time (getf right :accepted-at)))
    (or (< left-time right-time)
        (and (= left-time right-time)
             (string< (getf left :commit-id)
                      (getf right :commit-id))))))

(defun log-document (repository logical-id)
  "Return all known commits for LOGICAL-ID in chronological order."
  (sort
   (mapcar #'doc-value
           (index-fetch (repository-database repository)
                        +by-document-index+
                        logical-id))
   #'%commit<))

(defun inventory-commits (repository tenant dataset day dtype)
  "Return commits for one UTC tenant/dataset/day/dtype inventory key."
  (sort
   (mapcar #'doc-value
           (index-fetch (repository-database repository)
                        +by-dataset-day-index+
                        (%dataset-day-key tenant dataset day dtype)))
   #'%commit<))

(defun commits-for-blob (repository blob-id)
  "Return commit records referencing BLOB-ID."
  (sort
   (mapcar #'doc-value
           (index-fetch (repository-database repository)
                        +by-content-index+
                        blob-id))
   #'%commit<))

(defun fsck (repository)
  "Return a list of integrity problems. NIL means the checked core is healthy."
  (let ((database (repository-database repository))
        (problems nil))
    (map-database
     database
     :database-name +objects-db+
     :map-fn
     (lambda (id document)
       (let* ((value (doc-value document))
              (type (getf value :type))
              (bytes (getf value :bytes)))
         (cond
           ((or (not (stringp type))
                (not (%octet-vector-p bytes)))
            (push (list :invalid-object id) problems))
           ((not (string= id (%typed-object-id type bytes)))
            (push (list :hash-mismatch id) problems))))))
    (map-database
     database
     :database-name +commits-db+
     :map-fn
     (lambda (id document)
       (let* ((record (doc-value document))
              (blob-id (getf record :blob-id))
              (blob (and blob-id (read-object repository blob-id))))
         (unless (and blob (string= (getf blob :type) "blob"))
           (push (list :missing-blob id blob-id) problems))
         (dolist (parent (getf record :parents))
           (unless (read-commit repository parent)
             (push (list :missing-parent id parent) problems))))))
    (with-read-transaction (database)
      (lmdb:do-db (ref-name commit-id (%refs-db repository))
        (unless (read-commit repository commit-id)
          (push (list :dangling-ref ref-name commit-id) problems))))
    (nreverse problems)))
