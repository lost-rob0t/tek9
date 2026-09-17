(in-package :star-git)

(defparameter +packs-db+ "star-git/packs")
(defparameter +pack-index-db+ "star-git/pack-index")
(defparameter +pack-magic+
  (make-array 4
              :element-type '(unsigned-byte 8)
              :initial-contents '(83 71 80 75))) ; SGPK
(defconstant +pack-format-version+ 1)
(defparameter +pack-codec+ "lzma")
(defparameter +default-max-pack-objects+ 10000000)
(defparameter +default-max-object-bytes+ (* 256 1024 1024))
(defparameter +default-max-compressed-object-bytes+ (* 300 1024 1024))

(define-condition pack-error (error)
  ((pathname :initarg :pathname :reader pack-error-pathname)
   (reason :initarg :reason :reader pack-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "star-git pack error for ~A: ~A"
             (pack-error-pathname condition)
             (pack-error-reason condition)))))

(define-condition pack-integrity-error (pack-error)
  ((object-id :initarg :object-id :initform nil :reader pack-integrity-object-id)))

(defun %pack-fail (pathname control &rest arguments)
  (error 'pack-error
         :pathname pathname
         :reason (apply #'format nil control arguments)))

(defun %pack-integrity-fail (pathname object-id control &rest arguments)
  (error 'pack-integrity-error
         :pathname pathname
         :object-id object-id
         :reason (apply #'format nil control arguments)))

(defun %write-u16 (value stream)
  (check-type value (integer 0 65535))
  (write-byte (ldb (byte 8 8) value) stream)
  (write-byte (ldb (byte 8 0) value) stream))

(defun %write-u64 (value stream)
  (check-type value (integer 0 #xffffffffffffffff))
  (loop for shift from 56 downto 0 by 8
        do (write-byte (ldb (byte 8 shift) value) stream)))

(defun %read-required-byte (stream pathname)
  (let ((value (read-byte stream nil :eof)))
    (when (eq value :eof)
      (%pack-fail pathname "unexpected end of file"))
    value))

(defun %read-u16 (stream pathname)
  (logior (ash (%read-required-byte stream pathname) 8)
          (%read-required-byte stream pathname)))

(defun %read-u64 (stream pathname)
  (loop with value = 0
        repeat 8
        do (setf value
                 (logior (ash value 8)
                         (%read-required-byte stream pathname)))
        finally (return value)))

(defun %write-octets (octets stream)
  (write-sequence (%ensure-octets octets) stream))

(defun %read-octets (stream count pathname)
  (check-type count (integer 0 *))
  (let ((octets (make-array count :element-type '(unsigned-byte 8))))
    (unless (= (read-sequence octets stream) count)
      (%pack-fail pathname "truncated payload; expected ~D bytes" count))
    octets))

(defun %string-octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun %write-sized-string (string stream)
  (let ((octets (%string-octets string)))
    (when (> (length octets) 65535)
      (error "star-git pack string exceeds 65535 encoded bytes."))
    (%write-u16 (length octets) stream)
    (%write-octets octets stream)))

(defun %read-sized-string (stream pathname)
  (babel:octets-to-string
   (%read-octets stream (%read-u16 stream pathname) pathname)
   :encoding :utf-8))

(defun %write-pack-header (stream object-count)
  (%write-octets +pack-magic+ stream)
  (%write-u16 +pack-format-version+ stream)
  (%write-u16 0 stream)
  (%write-u64 object-count stream))

(defun %read-pack-header (stream pathname max-objects)
  (let ((magic (%read-octets stream (length +pack-magic+) pathname))
        (version (%read-u16 stream pathname))
        (flags (%read-u16 stream pathname))
        (count (%read-u64 stream pathname)))
    (unless (equalp magic +pack-magic+)
      (%pack-fail pathname "bad magic"))
    (unless (= version +pack-format-version+)
      (%pack-fail pathname "unsupported format version ~D" version))
    (unless (zerop flags)
      (%pack-fail pathname "unsupported flags ~D" flags))
    (when (> count max-objects)
      (%pack-fail pathname "object count ~D exceeds limit ~D" count max-objects))
    count))

(defun %typed-file-id (type pathname)
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let* ((size (file-length stream))
           (digest (ironclad:make-digest :sha256))
           (header (%string-octets
                    (format nil "star-git ~A ~D~C" type size #\Null)))
           (buffer (make-array (* 1024 1024)
                               :element-type '(unsigned-byte 8))))
      (ironclad:update-digest digest header)
      (loop for count = (read-sequence buffer stream)
            while (plusp count)
            do (ironclad:update-digest
                digest
                (if (= count (length buffer))
                    buffer
                    (subseq buffer 0 count))))
      (format nil "sha256:~A"
              (ironclad:byte-array-to-hex-string
               (ironclad:produce-digest digest))))))

(defun pack-id-for-path (pathname)
  "Return the typed content id for a complete immutable pack file."
  (%typed-file-id "pack" pathname))

(defun %temporary-pack-pathname (pathname)
  (make-pathname
   :name (format nil ".~A-~D-~8,'0X"
                 (or (pathname-name pathname) "star-git-pack")
                 (get-universal-time)
                 (random #x100000000))
   :type "tmp"
   :defaults pathname))

(defun %packs-db (repository)
  (database-db (repository-database repository)
               +packs-db+
               :key-encoding :utf-8
               :value-encoding :octets))

(defun %pack-index-db (repository)
  (database-db (repository-database repository)
               +pack-index-db+
               :key-encoding :utf-8
               :value-encoding :octets))

(defun read-pack-metadata (repository pack-id)
  (fetch* (repository-database repository)
          pack-id
          :database-name +packs-db+))

(defun pack-locations (repository object-id)
  "Return verified pack locators known for OBJECT-ID, newest first."
  (copy-list
   (or (fetch* (repository-database repository)
               object-id
               :database-name +pack-index-db+)
       nil)))

(defun %same-pack-metadata-p (left right)
  (and (= (getf left :format-version) (getf right :format-version))
       (string= (getf left :codec) (getf right :codec))
       (= (getf left :object-count) (getf right :object-count))
       (= (getf left :byte-length) (getf right :byte-length))))

(defun %register-pack (repository pathname pack-id locators object-count)
  (let* ((database (repository-database repository))
         (path (namestring pathname))
         (byte-length (with-open-file (stream pathname
                                              :direction :input
                                              :element-type '(unsigned-byte 8))
                        (file-length stream)))
         (base-metadata
           (list :pack-id pack-id
                 :format-version +pack-format-version+
                 :codec +pack-codec+
                 :object-count object-count
                 :byte-length byte-length)))
    (%packs-db repository)
    (%pack-index-db repository)
    (with-write-transaction
        (database :database-names (list +packs-db+ +pack-index-db+))
      (let* ((existing (read-pack-metadata repository pack-id))
             (metadata
               (if existing
                   (progn
                     (unless (%same-pack-metadata-p existing base-metadata)
                       (%pack-integrity-fail
                        pathname nil "pack metadata collision for ~A" pack-id))
                     (copy-list existing))
                   (append base-metadata
                           (list :local-paths nil :verified-at (get-universal-time))))))
        (setf (getf metadata :local-paths)
              (adjoin path (getf metadata :local-paths) :test #'string=)
              (getf metadata :verified-at) (get-universal-time))
        (put* database metadata :id pack-id :database-name +packs-db+)
        (dolist (locator locators)
          (let* ((object-id (getf locator :object-id))
                 (existing-locators
                   (or (fetch* database object-id
                               :database-name +pack-index-db+)
                       nil))
                 (locator
                   (list* :pack-id pack-id
                          (loop for (key value) on locator by #'cddr
                                unless (eq key :object-id)
                                  append (list key value))))
                 (without-this-pack
                   (remove pack-id existing-locators
                           :test #'string=
                           :key (lambda (item) (getf item :pack-id)))))
            (put* database
                  (cons locator without-this-pack)
                  :id object-id
                  :database-name +pack-index-db+)))
        metadata))))

(defun build-pack (repository object-ids pathname)
  "Build one deterministic immutable pack from OBJECT-IDS.

Objects are sorted by content id and deduplicated before writing. Each object is
compressed independently, allowing a pack index to seek to one object without
inflating the rest of the archive. The returned pack id hashes the complete
pack bytes and is independent from the source pathname."
  (let* ((pathname (pathname pathname))
         (object-ids (sort (remove-duplicates (copy-list object-ids)
                                              :test #'string=)
                           #'string<))
         (temporary (%temporary-pack-pathname pathname))
         (locators nil))
    (uiop:ensure-all-directories-exist (list pathname))
    (unwind-protect
         (progn
           (with-open-file (stream temporary
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :element-type '(unsigned-byte 8))
             (%write-pack-header stream (length object-ids))
             (dolist (object-id object-ids)
               (let ((object (read-object repository object-id)))
                 (unless object
                   (error 'missing-object :id object-id))
                 (let* ((type (getf object :type))
                        (bytes (%ensure-octets (getf object :bytes)))
                        (compressed (%lzma-compress bytes))
                        (offset (file-position stream)))
                   (%write-sized-string object-id stream)
                   (%write-sized-string type stream)
                   (%write-u64 (length bytes) stream)
                   (%write-u64 (length compressed) stream)
                   (%write-octets compressed stream)
                   (push (list :object-id object-id
                               :offset offset
                               :type type
                               :uncompressed-length (length bytes)
                               :compressed-length (length compressed)
                               :codec +pack-codec+)
                         locators)))))
           (when (probe-file pathname)
             (delete-file pathname))
           (rename-file temporary pathname)
           (setf temporary nil)
           (let* ((pack-id (pack-id-for-path pathname))
                  (metadata (%register-pack repository
                                            pathname
                                            pack-id
                                            (nreverse locators)
                                            (length object-ids))))
             (values pack-id metadata)))
      (when (and temporary (probe-file temporary))
        (ignore-errors (delete-file temporary))))))

(defun %octet-line (octets cursor pathname)
  (let ((end (position 10 octets :start cursor)))
    (unless end
      (%pack-fail pathname "truncated commit preimage"))
    (values (babel:octets-to-string octets
                                    :start cursor
                                    :end end
                                    :encoding :utf-8)
            (1+ end))))

(defun %commit-field (octets cursor expected-name pathname)
  (let ((colon (position 58 octets :start cursor)))
    (unless colon
      (%pack-fail pathname "invalid commit field ~A" expected-name))
    (let* ((prefix (babel:octets-to-string octets
                                           :start cursor
                                           :end colon
                                           :encoding :utf-8))
           (space (position #\Space prefix :from-end t)))
      (unless space
        (%pack-fail pathname "invalid commit field prefix ~S" prefix))
      (let* ((name (subseq prefix 0 space))
             (size (parse-integer prefix :start (1+ space) :junk-allowed nil))
             (start (1+ colon))
             (end (+ start size)))
        (unless (string= name expected-name)
          (%pack-fail pathname "expected commit field ~A, got ~A"
                      expected-name name))
        (unless (and (< end (length octets)) (= (aref octets end) 10))
          (%pack-fail pathname "truncated commit field ~A" expected-name))
        (values (babel:octets-to-string octets
                                        :start start
                                        :end end
                                        :encoding :utf-8)
                (1+ end))))))

(defun %commit-record-from-preimage (commit-id octets pathname)
  (multiple-value-bind (version cursor)
      (%octet-line octets 0 pathname)
    (unless (string= version "star-git-commit-v1")
      (%pack-fail pathname "unsupported commit preimage ~S" version))
    (multiple-value-bind (logical-id cursor)
        (%commit-field octets cursor "logical-id" pathname)
      (multiple-value-bind (blob-id cursor)
          (%commit-field octets cursor "blob-id" pathname)
        (multiple-value-bind (tenant cursor)
            (%commit-field octets cursor "tenant" pathname)
          (multiple-value-bind (dataset cursor)
              (%commit-field octets cursor "dataset" pathname)
            (multiple-value-bind (dtype cursor)
                (%commit-field octets cursor "dtype" pathname)
              (multiple-value-bind (mutation cursor)
                  (%commit-field octets cursor "mutation" pathname)
                (multiple-value-bind (accepted-at-string cursor)
                    (%commit-field octets cursor "accepted-at" pathname)
                  (multiple-value-bind (parents-line cursor)
                      (%octet-line octets cursor pathname)
                    (unless (and (>= (length parents-line) 8)
                                 (string= "parents " parents-line :end2 8))
                      (%pack-fail pathname "invalid parents header ~S" parents-line))
                    (let ((parent-count
                            (parse-integer parents-line
                                           :start 8
                                           :junk-allowed nil))
                          (parents nil))
                      (dotimes (index parent-count)
                        (declare (ignore index))
                        (multiple-value-bind (parent next-cursor)
                            (%commit-field octets cursor "parent" pathname)
                          (push parent parents)
                          (setf cursor next-cursor)))
                      (unless (= cursor (length octets))
                        (%pack-fail pathname "trailing bytes in commit preimage"))
                      (%commit-record commit-id
                                      logical-id
                                      blob-id
                                      (nreverse parents)
                                      tenant
                                      dataset
                                      dtype
                                      mutation
                                      (parse-integer accepted-at-string
                                                     :junk-allowed nil)))))))))))))

(defun %install-imported-commit (repository commit-id preimage pathname)
  (let* ((database (repository-database repository))
         (record (%commit-record-from-preimage commit-id preimage pathname))
         (existing (read-commit repository commit-id)))
    (cond
      ((null existing)
       (put* database record :id commit-id :database-name +commits-db+))
      ((not (equal existing record))
       (%pack-integrity-fail pathname commit-id
                             "commit metadata disagrees with existing index")))
    record))

(defun import-pack (repository pathname
                    &key
                      (max-objects +default-max-pack-objects+)
                      (max-object-bytes +default-max-object-bytes+)
                      (max-compressed-object-bytes
                        +default-max-compressed-object-bytes+))
  "Verify and import every immutable object from PATHNAME.

Import is retry-safe. Every entry is decompressed, its typed object id is
recomputed, and only then is it admitted to the Tek9 object store. Commit
preimages also rebuild the compact commit/history index, but refs are never
guessed from a pack; snapshot/tree objects will own ref restoration."
  (let* ((pathname (pathname pathname))
         (pack-id (pack-id-for-path pathname))
         (locators nil)
         (object-count 0))
    (with-open-file (stream pathname
                            :direction :input
                            :element-type '(unsigned-byte 8))
      (setf object-count (%read-pack-header stream pathname max-objects))
      (dotimes (index object-count)
        (declare (ignore index))
        (let* ((offset (file-position stream))
               (object-id (%read-sized-string stream pathname))
               (type (%read-sized-string stream pathname))
               (uncompressed-length (%read-u64 stream pathname))
               (compressed-length (%read-u64 stream pathname)))
          (when (> uncompressed-length max-object-bytes)
            (%pack-fail pathname "object ~A exceeds uncompressed limit" object-id))
          (when (> compressed-length max-compressed-object-bytes)
            (%pack-fail pathname "object ~A exceeds compressed limit" object-id))
          (let* ((compressed (%read-octets stream compressed-length pathname))
                 (bytes (%lzma-decompress compressed uncompressed-length))
                 (computed-id (%typed-object-id type bytes)))
            (unless (string= object-id computed-id)
              (%pack-integrity-fail
               pathname object-id "typed object hash mismatch; got ~A" computed-id))
            (unless (string= object-id (%write-object repository type bytes))
              (%pack-integrity-fail pathname object-id
                                    "repository returned a different object id"))
            (when (string= type "commit")
              (%install-imported-commit repository object-id bytes pathname))
            (push (list :object-id object-id
                        :offset offset
                        :type type
                        :uncompressed-length uncompressed-length
                        :compressed-length compressed-length
                        :codec +pack-codec+)
                  locators))))
      (unless (eq (read-byte stream nil :eof) :eof)
        (%pack-fail pathname "trailing bytes after declared object count")))
    (let ((metadata (%register-pack repository
                                    pathname
                                    pack-id
                                    (nreverse locators)
                                    object-count)))
      (values pack-id metadata))))

(defun %fsck-packs (repository)
  (let ((database (repository-database repository))
        (problems nil))
    (%packs-db repository)
    (%pack-index-db repository)
    (map-database
     database
     :database-name +pack-index-db+
     :map-fn
     (lambda (object-id document)
       (dolist (locator (doc-value document))
         (let ((pack-id (getf locator :pack-id)))
           (unless (and (stringp pack-id)
                        (read-pack-metadata repository pack-id))
             (push (list :broken-pack-index object-id pack-id) problems))))))
    (nreverse problems)))
