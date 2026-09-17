(in-package :tek9-tests)

(in-suite :tek9-tests)

(defun star-git-read-range (pathname start length)
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (file-position stream start)
    (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
      (is (= length (read-sequence bytes stream)))
      bytes)))

(test star-git-random-access-reads-one-packed-object
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-random-local/"))
        (pack #P"/tmp/test-tek9-star-git-random-local/archive/local.sgp.lzma"))
    (unwind-protect
         (let* ((bytes-a (star-git-test-octets "{\"random\":\"a\"}"))
                (bytes-b (star-git-test-octets "{\"random\":\"b\"}"))
                (blob-a (star-git:write-blob repository bytes-a))
                (blob-b (star-git:write-blob repository bytes-b)))
           (multiple-value-bind (pack-id metadata)
               (star-git:build-pack repository (list blob-b blob-a) pack)
             (declare (ignore metadata))
             (is (equalp bytes-a
                         (getf (star-git:read-packed-object
                                repository blob-a :pack-id pack-id)
                               :bytes)))
             (is (equalp bytes-b
                         (getf (star-git:read-packed-object
                                repository blob-b :pack-id pack-id)
                               :bytes)))
             (is (eq :evicted
                     (star-git:evict-hot-object
                      repository blob-a :minimum-custody :verified-any)))
             (is (equalp bytes-a
                         (getf (star-git:read-packed-object
                                repository blob-a :pack-id pack-id)
                               :bytes)))))
      (star-git:close-repository repository))))

(test star-git-random-access-honors-exact-pack-selection
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-random-two-packs/"))
        (pack-a #P"/tmp/test-tek9-star-git-random-two-packs/archive/a.sgp.lzma")
        (pack-b #P"/tmp/test-tek9-star-git-random-two-packs/archive/b.sgp.lzma"))
    (unwind-protect
         (let* ((bytes (star-git-test-octets "{\"two-packs\":true}"))
                (blob (star-git:write-blob repository bytes)))
           (multiple-value-bind (pack-id-a ignored-a)
               (star-git:build-pack repository (list blob) pack-a)
             (declare (ignore ignored-a))
             (multiple-value-bind (pack-id-b ignored-b)
                 (star-git:build-pack repository (list blob) pack-b)
               (declare (ignore ignored-b))
               ;; Identical immutable object sets produce the same pack identity.
               (is (string= pack-id-a pack-id-b))
               (is (equalp bytes
                           (getf (star-git:read-packed-object
                                  repository blob :pack-id pack-id-a)
                                 :bytes)))
               (signals star-git:packed-object-error
                 (star-git:read-packed-object
                  repository blob :pack-id "sha256:not-a-pack")))))
      (star-git:close-repository repository))))

(test star-git-remote-range-plan-recovers-exact-object
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-random-remote/"))
        (pack #P"/tmp/test-tek9-star-git-random-remote/archive/remote.sgp.lzma"))
    (unwind-protect
         (let* ((bytes (star-git-test-octets "{\"remote-range\":true}"))
                (blob (star-git:write-blob repository bytes)))
           (multiple-value-bind (pack-id metadata)
               (star-git:build-pack repository (list blob) pack)
             (star-git:register-custody-receipt
              repository
              "range-receipt"
              pack-id
              :state :verified
              :backend "linode-archive"
              :catalog-locator "archive:range-pack"
              :byte-length (getf metadata :byte-length)
              :verified-at 4000000500
              :verification :full-readback)
             (let* ((range-plan
                      (star-git:plan-packed-object-range
                       repository blob :pack-id pack-id))
                    (header
                      (star-git-read-range
                       pack
                       (getf range-plan :header-range-start)
                       (getf range-plan :header-range-length)))
                    (payload-plan
                      (star-git:plan-packed-object-payload-range
                       range-plan header))
                    (payload
                      (star-git-read-range
                       pack
                       (getf payload-plan :payload-range-start)
                       (getf payload-plan :payload-range-length)))
                    (object
                      (star-git:decode-packed-object-payload
                       payload-plan payload)))
               (is (string= "range-receipt"
                            (getf range-plan :receipt-id)))
               (is (string= "linode-archive"
                            (getf range-plan :backend)))
               (is (equalp bytes (getf object :bytes)))
               (is (= (getf range-plan :compressed-length)
                      (getf payload-plan :payload-range-length))))))
      (star-git:close-repository repository))))

(test star-git-remote-range-plan-requires-verified-custody
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-random-unverified/"))
        (pack #P"/tmp/test-tek9-star-git-random-unverified/archive/unverified.sgp.lzma"))
    (unwind-protect
         (let* ((blob (star-git:write-blob
                       repository
                       (star-git-test-octets "{\"unverified\":true}"))))
           (multiple-value-bind (pack-id metadata)
               (star-git:build-pack repository (list blob) pack)
             (star-git:register-custody-receipt
              repository
              "not-verified"
              pack-id
              :state :stored-unverified
              :backend "linode-archive"
              :catalog-locator "archive:not-verified"
              :byte-length (getf metadata :byte-length)
              :verification :deferred-strong)
             (signals star-git:packed-object-error
               (star-git:plan-packed-object-range
                repository blob :pack-id pack-id))))
      (star-git:close-repository repository))))

(test star-git-random-access-rejects-corrupt-packed-payload
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-random-corrupt/"))
        (pack #P"/tmp/test-tek9-star-git-random-corrupt/archive/corrupt.sgp.lzma"))
    (unwind-protect
         (let ((blob (star-git:write-blob
                      repository
                      (star-git-test-octets "{\"corrupt-range\":true}"))))
           (star-git:build-pack repository (list blob) pack)
           (with-open-file (stream pack
                                   :direction :io
                                   :if-exists :overwrite
                                   :element-type '(unsigned-byte 8))
             (let ((position (1- (file-length stream))))
               (file-position stream position)
               (let ((byte (read-byte stream)))
                 (file-position stream position)
                 (write-byte (logxor byte 1) stream))))
           (signals error
             (star-git:read-packed-object repository blob)))
      (star-git:close-repository repository))))

(test star-git-random-access-enforces-declared-size-limits-before-decode
  (let ((plan (list :object-id "sha256:oversized"
                    :pack-id "sha256:pack"
                    :type "blob"
                    :compressed-length 1
                    :uncompressed-length 1000000)))
    (signals star-git:packed-object-error
      (star-git:decode-packed-object-payload
       plan
       (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)
       :max-object-bytes 10
       :max-compressed-bytes 10))))
