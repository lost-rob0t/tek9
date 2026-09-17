(in-package :star-git)

(defparameter +tree-format-version+ "star-git-tree-v1")
(defparameter +tag-format-version+ "star-git-tag-v1")

(define-condition duplicate-tree-ref (error)
  ((ref :initarg :ref :reader duplicate-tree-ref-ref))
  (:report
   (lambda (condition stream)
     (format stream "star-git tree contains duplicate logical ref ~S."
             (duplicate-tree-ref-ref condition)))))

(define-condition missing-tree-entry (error)
  ((tree-id :initarg :tree-id :reader missing-tree-entry-tree-id)
   (ref :initarg :ref :reader missing-tree-entry-ref))
  (:report
   (lambda (condition stream)
     (format stream "star-git tree ~S contains no entry for logical ref ~S."
             (missing-tree-entry-tree-id condition)
             (missing-tree-entry-ref condition)))))

(define-condition snapshot-format-error (error)
  ((object-id :initarg :object-id :reader snapshot-format-error-object-id)
   (reason :initarg :reason :reader snapshot-format-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "star-git snapshot object ~S is invalid: ~A"
             (snapshot-format-error-object-id condition)
             (snapshot-format-error-reason condition)))))

(defun %snapshot-format-fail (object-id control &rest arguments)
  (error 'snapshot-format-error
         :object-id object-id
         :reason (apply #'format nil control arguments)))

(defun %normalize-tree-entry (entry)
  (unless (consp entry)
    (error 'type-error :datum entry :expected-type 'cons))
  (let ((ref (car entry))
        (commit-id (cdr entry)))
    (check-type ref string)
    (check-type commit-id string)
    (cons ref commit-id)))

(defun %normalize-tree-entries (repository entries)
  (let ((sorted
          (sort (mapcar #'%normalize-tree-entry entries)
                #'string<
                :key #'car)))
    (loop for previous = nil then entry
          for entry in sorted
          when (and previous (string= (car previous) (car entry)))
            do (error 'duplicate-tree-ref :ref (car entry)))
    (dolist (entry sorted)
      (%require-commit repository (cdr entry)))
    sorted))

(defun %tree-preimage (entries)
  (babel:string-to-octets
   (with-output-to-string (stream)
     (write-line +tree-format-version+ stream)
     (format stream "entries ~D~%" (length entries))
     (dolist (entry entries)
       (%write-field stream "ref" (car entry))
       (%write-field stream "commit" (cdr entry))))
   :encoding :utf-8))

(defun write-tree (repository entries)
  "Store a deterministic immutable ref->commit tree and return its object id.

ENTRIES may arrive in any order. They are normalized lexically by logical ref,
duplicate refs are rejected, and every referenced commit must already exist."
  (let ((entries (%normalize-tree-entries repository entries)))
    (%write-object repository "tree" (%tree-preimage entries))))

(defun %snapshot-octet-line (octets cursor object-id)
  (let ((end (position 10 octets :start cursor)))
    (unless end
      (%snapshot-format-fail object-id "truncated line at byte ~D" cursor))
    (values (babel:octets-to-string octets
                                    :start cursor
                                    :end end
                                    :encoding :utf-8)
            (1+ end))))

(defun %snapshot-field (octets cursor expected-name object-id)
  (let ((colon (position 58 octets :start cursor)))
    (unless colon
      (%snapshot-format-fail object-id
                             "missing colon for field ~A"
                             expected-name))
    (let* ((prefix (babel:octets-to-string octets
                                           :start cursor
                                           :end colon
                                           :encoding :utf-8))
           (space (position #\Space prefix :from-end t)))
      (unless space
        (%snapshot-format-fail object-id "invalid field prefix ~S" prefix))
      (let* ((name (subseq prefix 0 space))
             (size
               (handler-case
                   (parse-integer prefix :start (1+ space) :junk-allowed nil)
                 (error ()
                   (%snapshot-format-fail object-id
                                          "invalid length in field prefix ~S"
                                          prefix))))
             (start (1+ colon))
             (end (+ start size)))
        (unless (string= name expected-name)
          (%snapshot-format-fail object-id
                                 "expected field ~A, got ~A"
                                 expected-name name))
        (unless (and (< end (length octets))
                     (= (aref octets end) 10))
          (%snapshot-format-fail object-id
                                 "truncated field ~A"
                                 expected-name))
        (values (babel:octets-to-string octets
                                        :start start
                                        :end end
                                        :encoding :utf-8)
                (1+ end))))))

(defun %parse-count-line (line prefix object-id)
  (unless (and (>= (length line) (length prefix))
               (string= prefix line :end2 (length prefix)))
    (%snapshot-format-fail object-id "invalid count header ~S" line))
  (handler-case
      (parse-integer line :start (length prefix) :junk-allowed nil)
    (error ()
      (%snapshot-format-fail object-id "invalid count header ~S" line))))

(defun %decode-tree (tree-id octets)
  (multiple-value-bind (version cursor)
      (%snapshot-octet-line octets 0 tree-id)
    (unless (string= version +tree-format-version+)
      (%snapshot-format-fail tree-id "unsupported tree version ~S" version))
    (multiple-value-bind (count-line cursor)
        (%snapshot-octet-line octets cursor tree-id)
      (let ((count (%parse-count-line count-line "entries " tree-id))
            (entries nil)
            (previous-ref nil))
        (dotimes (index count)
          (declare (ignore index))
          (multiple-value-bind (ref next-cursor)
              (%snapshot-field octets cursor "ref" tree-id)
            (setf cursor next-cursor)
            (multiple-value-bind (commit-id next-cursor)
                (%snapshot-field octets cursor "commit" tree-id)
              (setf cursor next-cursor)
              (when (and previous-ref
                         (not (string< previous-ref ref)))
                (%snapshot-format-fail
                 tree-id
                 "tree refs are not strictly sorted at ~S"
                 ref))
              (push (cons ref commit-id) entries)
              (setf previous-ref ref))))
        (unless (= cursor (length octets))
          (%snapshot-format-fail tree-id "trailing bytes after tree entries"))
        (nreverse entries)))))

(defun read-tree (repository tree-id &key (validate-commits t))
  "Decode TREE-ID into its canonical ordered ref->commit entries."
  (let ((object (read-object repository tree-id)))
    (unless object
      (error 'missing-object :id tree-id))
    (unless (string= (getf object :type) "tree")
      (error 'object-integrity-error :id tree-id))
    (let ((entries (%decode-tree tree-id (getf object :bytes))))
      (when validate-commits
        (dolist (entry entries)
          (%require-commit repository (cdr entry))))
      entries)))

(defun dataset-snapshot-ref-name (tenant dataset day)
  "Return the mutable ref name for one immutable tenant/dataset/day tree."
  (check-type day string)
  (format nil "refs/datasets/~A/~A/daily/~A"
          (%component tenant)
          (%component dataset)
          day))

(defun tag-ref-name (name)
  "Return the mutable ref name for a named immutable star-git tag object."
  (check-type name string)
  (format nil "refs/tags/~A" (%component name)))

(defun %require-object-type (repository object-id expected-type)
  (let ((object (read-object repository object-id)))
    (unless object
      (error 'missing-object :id object-id))
    (unless (string= (getf object :type) expected-type)
      (error 'object-integrity-error :id object-id))
    object-id))

(defun update-object-ref (repository ref-name new-object-id
                          &key
                            (expected :any)
                            expected-type)
  "CAS REF-NAME to an immutable star-git object.

Unlike UPDATE-REF, which intentionally accepts commit ids only, this operation
is for snapshot/tag refs whose targets are tree/tag objects."
  (if expected-type
      (%require-object-type repository new-object-id expected-type)
      (unless (read-object repository new-object-id)
        (error 'missing-object :id new-object-id)))
  (let ((database (repository-database repository)))
    (with-write-transaction (database)
      (let ((current (%resolve-ref-in-transaction repository ref-name)))
        (unless (or (eq expected :any)
                    (equal expected current))
          (error 'ref-conflict
                 :ref ref-name
                 :expected expected
                 :current current))
        (lmdb:put (%refs-db repository) ref-name new-object-id)
        new-object-id))))

(defun snapshot-dataset-day (repository tenant dataset day entries
                             &key (expected :any))
  "Write one immutable dataset/day tree and CAS its stable snapshot ref."
  (let* ((tree-id (write-tree repository entries))
         (ref-name (dataset-snapshot-ref-name tenant dataset day)))
    (update-object-ref repository
                       ref-name
                       tree-id
                       :expected expected
                       :expected-type "tree")
    (values tree-id ref-name)))

(defun restore-ref-from-tree (repository tree-id logical-ref
                              &key target-ref (expected :any))
  "Explicitly restore one commit ref from TREE-ID using normal CAS semantics.

Pack import never calls this operation automatically."
  (check-type logical-ref string)
  (let* ((entries (read-tree repository tree-id))
         (entry (assoc logical-ref entries :test #'string=)))
    (unless entry
      (error 'missing-tree-entry :tree-id tree-id :ref logical-ref))
    (update-ref repository
                (or target-ref logical-ref)
                (cdr entry)
                :expected expected)))

(defun %tag-preimage (name tree-id created-at note)
  (babel:string-to-octets
   (with-output-to-string (stream)
     (write-line +tag-format-version+ stream)
     (%write-field stream "name" name)
     (%write-field stream "tree" tree-id)
     (%write-field stream "created-at"
                   (write-to-string created-at :base 10 :radix nil))
     (%write-field stream "note" note))
   :encoding :utf-8))

(defun write-tag (repository name tree-id
                  &key
                    (created-at (get-universal-time))
                    (note "")
                    (expected :any)
                    (update-ref-p t))
  "Write an immutable named checkpoint object pointing at TREE-ID.

When UPDATE-REF-P is true, CAS refs/tags/<name> to the new tag object."
  (check-type name string)
  (check-type created-at integer)
  (check-type note string)
  (%require-object-type repository tree-id "tree")
  (let* ((tag-id
           (%write-object repository
                          "tag"
                          (%tag-preimage name tree-id created-at note)))
         (ref-name (tag-ref-name name)))
    (when update-ref-p
      (update-object-ref repository
                         ref-name
                         tag-id
                         :expected expected
                         :expected-type "tag"))
    (values tag-id ref-name)))

(defun %decode-tag (tag-id octets)
  (multiple-value-bind (version cursor)
      (%snapshot-octet-line octets 0 tag-id)
    (unless (string= version +tag-format-version+)
      (%snapshot-format-fail tag-id "unsupported tag version ~S" version))
    (multiple-value-bind (name cursor)
        (%snapshot-field octets cursor "name" tag-id)
      (multiple-value-bind (tree-id cursor)
          (%snapshot-field octets cursor "tree" tag-id)
        (multiple-value-bind (created-at-string cursor)
            (%snapshot-field octets cursor "created-at" tag-id)
          (multiple-value-bind (note cursor)
              (%snapshot-field octets cursor "note" tag-id)
            (unless (= cursor (length octets))
              (%snapshot-format-fail tag-id "trailing bytes after tag"))
            (list :name name
                  :tree-id tree-id
                  :created-at
                  (handler-case
                      (parse-integer created-at-string :junk-allowed nil)
                    (error ()
                      (%snapshot-format-fail tag-id
                                             "invalid created-at ~S"
                                             created-at-string)))
                  :note note)))))))

(defun read-tag (repository tag-id &key (validate-tree t))
  "Decode one immutable tag object."
  (let ((object (read-object repository tag-id)))
    (unless object
      (error 'missing-object :id tag-id))
    (unless (string= (getf object :type) "tag")
      (error 'object-integrity-error :id tag-id))
    (let ((tag (%decode-tag tag-id (getf object :bytes))))
      (when validate-tree
        (%require-object-type repository (getf tag :tree-id) "tree"))
      tag)))

(defun validate-snapshot-object (repository object-id)
  "Validate tree/tag referential integrity after pack import."
  (let ((object (read-object repository object-id)))
    (unless object
      (error 'missing-object :id object-id))
    (cond
      ((string= (getf object :type) "tree")
       (read-tree repository object-id :validate-commits t))
      ((string= (getf object :type) "tag")
       (read-tag repository object-id :validate-tree t))
      (t nil))))
