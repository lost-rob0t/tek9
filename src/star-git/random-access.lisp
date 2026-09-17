(in-package :star-git)

(defparameter +random-access-max-object-bytes+ +default-max-object-bytes+)
(defparameter +random-access-max-compressed-bytes+
  +default-max-compressed-object-bytes+)

(define-condition packed-object-error (error)
  ((object-id :initarg :object-id :reader packed-object-error-object-id)
   (pack-id :initarg :pack-id :initform nil :reader packed-object-error-pack-id)
   (reason :initarg :reason :reader packed-object-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "star-git packed object ~S~@[ in pack ~S~]: ~A"
             (packed-object-error-object-id condition)
             (packed-object-error-pack-id condition)
             (packed-object-error-reason condition)))))

(defun %packed-object-error (object-id pack-id control &rest arguments)
  (error 'packed-object-error
         :object-id object-id
         :pack-id pack-id
         :reason (apply #'format nil control arguments)))

(defun %locator-pack-id (locator)
  (getf locator :pack-id))

(defun %select-pack-locator (repository object-id &key pack-id)
  (let* ((locators (pack-locations repository object-id))
         (locator
           (if pack-id
               (find pack-id locators :test #'string=
                                  :key #'%locator-pack-id)
               (first locators))))
    (unless locator
      (%packed-object-error object-id pack-id
                            "no indexed pack locator is available"))
    locator))

(defun %local-path-for-pack (repository object-id pack-id)
  (let ((metadata (read-pack-metadata repository pack-id)))
    (unless metadata
      (%packed-object-error object-id pack-id "pack metadata is missing"))
    (or (loop for path in (getf metadata :local-paths)
              for existing = (and path (probe-file path))
              when existing
                return existing)
        (%packed-object-error object-id pack-id
                              "no present local pack path is available"))))

(defun %entry-header-length-from-locator (locator)
  (let* ((object-id (getf locator :object-id))
         (type (getf locator :type))
         (object-id-octets (%string-octets object-id))
         (type-octets (%string-octets type)))
    (+ 2 (length object-id-octets)
       2 (length type-octets)
       8 8)))

(defun %validate-locator-lengths (object-id pack-id locator
                                  max-object-bytes max-compressed-bytes)
  (let ((uncompressed (getf locator :uncompressed-length))
        (compressed (getf locator :compressed-length)))
    (unless (and (integerp uncompressed) (not (minusp uncompressed)))
      (%packed-object-error object-id pack-id
                            "invalid uncompressed length ~S" uncompressed))
    (unless (and (integerp compressed) (not (minusp compressed)))
      (%packed-object-error object-id pack-id
                            "invalid compressed length ~S" compressed))
    (when (> uncompressed max-object-bytes)
      (%packed-object-error object-id pack-id
                            "uncompressed length ~D exceeds limit ~D"
                            uncompressed max-object-bytes))
    (when (> compressed max-compressed-bytes)
      (%packed-object-error object-id pack-id
                            "compressed length ~D exceeds limit ~D"
                            compressed max-compressed-bytes))
    (values uncompressed compressed)))

(defun %read-range-octets (pathname start length object-id pack-id)
  (check-type start (integer 0 *))
  (check-type length (integer 0 *))
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((file-size (file-length stream))
          (end (+ start length)))
      (when (> end file-size)
        (%packed-object-error object-id pack-id
                              "range [~D,~D) exceeds local pack size ~D"
                              start end file-size))
      (file-position stream start)
      (%read-octets stream length pathname))))

(defun %parse-entry-header-octets (object-id pack-id locator header-octets)
  (let ((cursor 0)
        (length (length header-octets)))
    (labels ((required-byte ()
               (when (>= cursor length)
                 (%packed-object-error object-id pack-id
                                       "truncated packed-object header"))
               (prog1 (aref header-octets cursor)
                 (incf cursor)))
             (u16 ()
               (logior (ash (required-byte) 8)
                       (required-byte)))
             (u64 ()
               (loop with value = 0
                     repeat 8
                     do (setf value
                              (logior (ash value 8)
                                      (required-byte)))
                     finally (return value)))
             (string-field ()
               (let* ((size (u16))
                      (end (+ cursor size)))
                 (when (> end length)
                   (%packed-object-error object-id pack-id
                                         "truncated packed-object string field"))
                 (prog1
                     (babel:octets-to-string header-octets
                                             :start cursor
                                             :end end
                                             :encoding :utf-8)
                   (setf cursor end)))))
      (let ((parsed-object-id (string-field))
            (parsed-type (string-field))
            (uncompressed (u64))
            (compressed (u64)))
        (unless (= cursor length)
          (%packed-object-error object-id pack-id
                                "header length mismatch: parsed ~D of ~D bytes"
                                cursor length))
        (unless (string= parsed-object-id object-id)
          (%packed-object-error object-id pack-id
                                "entry object id is ~S" parsed-object-id))
        (unless (string= parsed-type (getf locator :type))
          (%packed-object-error object-id pack-id
                                "entry type is ~S, index expects ~S"
                                parsed-type (getf locator :type)))
        (unless (= uncompressed (getf locator :uncompressed-length))
          (%packed-object-error object-id pack-id
                                "entry uncompressed length ~D disagrees with index ~D"
                                uncompressed
                                (getf locator :uncompressed-length)))
        (unless (= compressed (getf locator :compressed-length))
          (%packed-object-error object-id pack-id
                                "entry compressed length ~D disagrees with index ~D"
                                compressed
                                (getf locator :compressed-length)))
        (list :object-id object-id
              :pack-id pack-id
              :type parsed-type
              :entry-offset (getf locator :offset)
              :header-length length
              :payload-offset (+ (getf locator :offset) length)
              :compressed-length compressed
              :uncompressed-length uncompressed
              :codec (getf locator :codec))))))

(defun decode-packed-object-payload (plan compressed-octets
                                     &key
                                       (max-object-bytes
                                         +random-access-max-object-bytes+)
                                       (max-compressed-bytes
                                         +random-access-max-compressed-bytes+))
  "Verify/decode COMPRESSED-OCTETS according to a parsed packed-object PLAN."
  (let ((object-id (getf plan :object-id))
        (pack-id (getf plan :pack-id))
        (compressed-length (getf plan :compressed-length))
        (uncompressed-length (getf plan :uncompressed-length))
        (type (getf plan :type)))
    (%validate-locator-lengths object-id pack-id plan
                               max-object-bytes max-compressed-bytes)
    (unless (= (length compressed-octets) compressed-length)
      (%packed-object-error object-id pack-id
                            "payload length ~D does not equal expected ~D"
                            (length compressed-octets) compressed-length))
    (let* ((bytes (%lzma-decompress (%ensure-octets compressed-octets)
                                    uncompressed-length))
           (computed-id (%typed-object-id type bytes)))
      (unless (string= object-id computed-id)
        (%packed-object-error object-id pack-id
                              "typed object hash mismatch: ~A" computed-id))
      (%object-value type bytes))))

(defun read-packed-object (repository object-id
                           &key
                             pack-id
                             (max-object-bytes
                               +random-access-max-object-bytes+)
                             (max-compressed-bytes
                               +random-access-max-compressed-bytes+))
  "Read and verify OBJECT-ID from one present local immutable pack.

This operation does not import/cache the object or mutate refs/placement."
  (let* ((locator (%select-pack-locator repository object-id :pack-id pack-id))
         (pack-id (%locator-pack-id locator))
         (pathname (%local-path-for-pack repository object-id pack-id))
         (header-length (%entry-header-length-from-locator locator)))
    (%validate-locator-lengths object-id pack-id locator
                               max-object-bytes max-compressed-bytes)
    (let* ((header (%read-range-octets pathname
                                       (getf locator :offset)
                                       header-length
                                       object-id pack-id))
           (plan (%parse-entry-header-octets object-id pack-id locator header))
           (payload (%read-range-octets pathname
                                        (getf plan :payload-offset)
                                        (getf plan :compressed-length)
                                        object-id pack-id)))
      (decode-packed-object-payload plan payload
                                    :max-object-bytes max-object-bytes
                                    :max-compressed-bytes max-compressed-bytes))))

(defun %verified-remote-location-for-pack (repository pack-id)
  (first (verified-custody-receipts-for-pack repository pack-id)))

(defun plan-packed-object-range (repository object-id
                                 &key
                                   pack-id
                                   (minimum-custody :verified-remote)
                                   (max-object-bytes
                                     +random-access-max-object-bytes+)
                                   (max-compressed-bytes
                                     +random-access-max-compressed-bytes+))
  "Return a no-I/O two-stage range-read plan for a packed immutable object."
  (unless (eq minimum-custody :verified-remote)
    (%packed-object-error object-id pack-id
                          "unsupported remote minimum custody ~S"
                          minimum-custody))
  (let* ((locator (%select-pack-locator repository object-id :pack-id pack-id))
         (pack-id (%locator-pack-id locator))
         (receipt (%verified-remote-location-for-pack repository pack-id)))
    (unless receipt
      (%packed-object-error object-id pack-id
                            "no verified remote custody receipt is available"))
    (%validate-locator-lengths object-id pack-id locator
                               max-object-bytes max-compressed-bytes)
    (let ((header-length (%entry-header-length-from-locator locator)))
      (list :object-id object-id
            :pack-id pack-id
            :type (getf locator :type)
            :receipt-id (getf receipt :receipt-id)
            :backend (getf receipt :backend)
            :catalog-locator (getf receipt :catalog-locator)
            :entry-offset (getf locator :offset)
            :header-range-start (getf locator :offset)
            :header-range-length header-length
            :compressed-length (getf locator :compressed-length)
            :uncompressed-length (getf locator :uncompressed-length)
            :codec (getf locator :codec)))))

(defun plan-packed-object-payload-range (range-plan header-octets)
  "Validate HEADER-OCTETS and return the exact second-stage payload range."
  (let* ((object-id (getf range-plan :object-id))
         (pack-id (getf range-plan :pack-id))
         (locator
           (list :object-id object-id
                 :pack-id pack-id
                 :type (getf range-plan :type)
                 :offset (getf range-plan :entry-offset)
                 :uncompressed-length (getf range-plan :uncompressed-length)
                 :compressed-length (getf range-plan :compressed-length)
                 :codec (getf range-plan :codec)))
         (parsed (%parse-entry-header-octets object-id pack-id locator
                                             (%ensure-octets header-octets))))
    (append parsed
            (list :payload-range-start (getf parsed :payload-offset)
                  :payload-range-length (getf parsed :compressed-length)
                  :receipt-id (getf range-plan :receipt-id)
                  :backend (getf range-plan :backend)
                  :catalog-locator (getf range-plan :catalog-locator)))))
