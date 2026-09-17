(in-package :star-git)

(defparameter +default-import-batch-object-limit+ 4096)
(defparameter +default-import-batch-byte-limit+ (* 64 1024 1024))

(defvar *import-pack-batch-observer* nil
  "Optional test/benchmark hook called after each committed import/index batch.")

(defun %positive-import-limit (value name)
  (declare (ignore name))
  (unless (and (integerp value) (plusp value))
    (error 'type-error :datum value :expected-type '(integer 1 *)))
  value)

(defun %notify-import-batch (phase count byte-count)
  (when *import-pack-batch-observer*
    (funcall *import-pack-batch-observer* phase count byte-count)))

(defun %prepare-import-write-dbis (repository)
  "Open every DBI that an object batch may touch before its LMDB transaction."
  (let ((database (repository-database repository)))
    (database-db database +objects-db+
                 :key-encoding :utf-8
                 :value-encoding :octets)
    (database-db database +commits-db+
                 :key-encoding :utf-8
                 :value-encoding :octets)
    (when (fboundp '%ensure-v2-index)
      (funcall (symbol-function '%ensure-v2-index) repository))
    database))

(defun %flush-import-object-batch (repository pathname batch decoded-bytes)
  (when batch
    (let ((database (%prepare-import-write-dbis repository))
          (entries (nreverse batch)))
      (with-write-transaction
          (database :database-names (list +objects-db+ +commits-db+))
        (dolist (entry entries)
          (let ((object-id (getf entry :object-id))
                (type (getf entry :type))
                (bytes (getf entry :bytes)))
            (unless (string= object-id (%write-object repository type bytes))
              (%pack-integrity-fail pathname object-id
                                    "repository returned a different object id"))
            (when (string= type "commit")
              (%install-imported-commit repository object-id bytes pathname)))))
      (%notify-import-batch :objects (length entries) decoded-bytes))))

(defun %index-pack-locator-batch (repository pack-id locators)
  (when locators
    (let ((database (repository-database repository))
          (locators (nreverse locators)))
      (%pack-index-db repository)
      (with-write-transaction
          (database :database-names (list +pack-index-db+))
        (dolist (locator locators)
          (let* ((object-id (getf locator :object-id))
                 (existing-locators
                   (or (fetch* database object-id
                               :database-name +pack-index-db+)
                       nil))
                 (stored-locator
                   (list* :pack-id pack-id
                          (loop for (key value) on locator by #'cddr
                                unless (eq key :object-id)
                                  append (list key value))))
                 (without-this-pack
                   (remove pack-id existing-locators
                           :test #'string=
                           :key (lambda (item) (getf item :pack-id)))))
            (put* database
                  (cons stored-locator without-this-pack)
                  :id object-id
                  :database-name +pack-index-db+))))
      (%notify-import-batch :locators (length locators) 0))))

(defun %skip-pack-payload (stream compressed-length pathname)
  (let* ((current (file-position stream))
         (target (+ current compressed-length))
         (length (file-length stream)))
    (when (> target length)
      (%pack-fail pathname
                  "truncated payload; expected ~D compressed bytes"
                  compressed-length))
    (unless (file-position stream target)
      (%pack-fail pathname "unable to seek over compressed payload"))))

(defun %index-verified-pack (repository pathname pack-id object-count batch-limit)
  "Second pass: index offsets after the complete pack has already verified."
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((declared-count
            (%read-pack-header stream pathname object-count))
          (batch nil))
      (unless (= declared-count object-count)
        (%pack-fail pathname
                    "object count changed between verification and indexing"))
      (dotimes (index object-count)
        (declare (ignore index))
        (let* ((offset (file-position stream))
               (object-id (%read-sized-string stream pathname))
               (type (%read-sized-string stream pathname))
               (uncompressed-length (%read-u64 stream pathname))
               (compressed-length (%read-u64 stream pathname)))
          (%skip-pack-payload stream compressed-length pathname)
          (push (list :object-id object-id
                      :offset offset
                      :type type
                      :uncompressed-length uncompressed-length
                      :compressed-length compressed-length
                      :codec +pack-codec+)
                batch)
          (when (>= (length batch) batch-limit)
            (%index-pack-locator-batch repository pack-id batch)
            (setf batch nil))))
      (%index-pack-locator-batch repository pack-id batch)
      (unless (eq (read-byte stream nil :eof) :eof)
        (%pack-fail pathname "trailing bytes after declared object count")))))

(defun import-pack-batched (repository pathname
                            &key
                              (max-objects +default-max-pack-objects+)
                              (max-object-bytes +default-max-object-bytes+)
                              (max-compressed-object-bytes
                                +default-max-compressed-object-bytes+)
                              (batch-object-limit
                                +default-import-batch-object-limit+)
                              (batch-byte-limit
                                +default-import-batch-byte-limit+))
  "Verify and import PATHNAME using bounded fully durable LMDB batches.

Pass one streams, decompresses and hashes every object. Verified objects are
committed in bounded batches; a crash may therefore leave earlier immutable
objects installed, and retry safely reuses them. Pack metadata is not registered
until the entire input has verified and reached EOF.

Pass two re-reads only entry headers and seeks over compressed payloads to build
object->pack indexes in bounded transactions without decompressing twice."
  (%positive-import-limit batch-object-limit 'batch-object-limit)
  (%positive-import-limit batch-byte-limit 'batch-byte-limit)
  (let* ((pathname (pathname pathname))
         (pack-id (pack-id-for-path pathname))
         (object-count 0)
         (batch nil)
         (batch-count 0)
         (batch-bytes 0))
    (labels ((flush-batch ()
               (when batch
                 (%flush-import-object-batch
                  repository pathname batch batch-bytes)
                 (setf batch nil
                       batch-count 0
                       batch-bytes 0))))
      (with-open-file (stream pathname
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (setf object-count (%read-pack-header stream pathname max-objects))
        (dotimes (index object-count)
          (declare (ignore index))
          (let* ((object-id (%read-sized-string stream pathname))
                 (type (%read-sized-string stream pathname))
                 (uncompressed-length (%read-u64 stream pathname))
                 (compressed-length (%read-u64 stream pathname)))
            (when (> uncompressed-length max-object-bytes)
              (%pack-fail pathname
                          "object ~A exceeds uncompressed limit"
                          object-id))
            (when (> compressed-length max-compressed-object-bytes)
              (%pack-fail pathname
                          "object ~A exceeds compressed limit"
                          object-id))
            (when (and batch
                       (> (+ batch-bytes uncompressed-length)
                          batch-byte-limit))
              (flush-batch))
            (let* ((compressed
                     (%read-octets stream compressed-length pathname))
                   (bytes
                     (%lzma-decompress compressed uncompressed-length))
                   (computed-id (%typed-object-id type bytes)))
              (unless (string= object-id computed-id)
                (%pack-integrity-fail
                 pathname object-id
                 "typed object hash mismatch; got ~A"
                 computed-id))
              (push (list :object-id object-id :type type :bytes bytes) batch)
              (incf batch-count)
              (incf batch-bytes uncompressed-length)
              (when (or (>= batch-count batch-object-limit)
                        (>= batch-bytes batch-byte-limit))
                (flush-batch)))))
        (unless (eq (read-byte stream nil :eof) :eof)
          (%pack-fail pathname "trailing bytes after declared object count"))
        (flush-batch)))
    (let ((metadata (%register-pack repository pathname pack-id nil object-count)))
      (%index-verified-pack repository
                            pathname
                            pack-id
                            object-count
                            batch-object-limit)
      (values pack-id metadata))))

(setf (symbol-function 'import-pack) #'import-pack-batched)
