(in-package :tek9-tests)

(in-suite :tek9-tests)

(defun setup-star-git-repository (path)
  (when (uiop:directory-exists-p path)
    (uiop:delete-directory-tree path :validate t))
  (star-git:open-repository path))

(defun star-git-test-octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun star-git-read-file-octets (pathname)
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(test star-git-preserves-document-revision-chain
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-history/")))
    (unwind-protect
         (let* ((accepted-at (encode-universal-time 0 0 0 17 9 2026 0))
                (blob-1 (star-git:write-blob
                         repository
                         (star-git-test-octets "{\"value\":1}")))
                (commit-1 (star-git:commit-document
                           repository
                           "doc-1"
                           blob-1
                           :tenant "tenant-a"
                           :dataset "dataset-a"
                           :dtype "person"
                           :mutation-kind "create"
                           :accepted-at accepted-at))
                (blob-2 (star-git:write-blob
                         repository
                         (star-git-test-octets "{\"value\":2}")))
                (commit-2 (star-git:commit-document
                           repository
                           "doc-1"
                           blob-2
                           :tenant "tenant-a"
                           :dataset "dataset-a"
                           :dtype "person"
                           :mutation-kind "update"
                           :accepted-at (1+ accepted-at)))
                (ref (star-git:document-ref-name
                      "doc-1"
                      :tenant "tenant-a"
                      :dataset "dataset-a"))
                (history (star-git:log-document repository "doc-1")))
           (is (not (string= commit-1 commit-2)))
           (is (string= commit-2 (star-git:resolve-ref repository ref)))
           (is (= 2 (length history)))
           (is (equal nil (getf (first history) :parents)))
           (is (equal (list commit-1) (getf (second history) :parents)))
           (is (equalp (star-git-test-octets "{\"value\":1}")
                       (star-git:read-blob repository blob-1)))
           (is (= 2
                  (length
                   (star-git:inventory-commits repository
                                               "tenant-a"
                                               "dataset-a"
                                               "2026-09-17"
                                               "person"))))
           (is (null (star-git:fsck repository))))
      (star-git:close-repository repository))))

(test star-git-deduplicates-identical-blobs
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-dedupe/")))
    (unwind-protect
         (let* ((bytes (star-git-test-octets "{\"same\":true}"))
                (first-id (star-git:write-blob repository bytes))
                (second-id (star-git:write-blob repository bytes)))
           (is (string= first-id second-id))
           (is (equalp bytes (star-git:read-blob repository first-id)))
           (is (null (star-git:fsck repository))))
      (star-git:close-repository repository))))

(test star-git-ref-update-is-compare-and-swap
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-cas/")))
    (unwind-protect
         (let* ((accepted-at (encode-universal-time 0 0 0 17 9 2026 0))
                (blob-1 (star-git:write-blob
                         repository
                         (star-git-test-octets "{\"value\":1}")))
                (commit-1 (star-git:commit-document
                           repository
                           "doc-cas"
                           blob-1
                           :tenant "tenant-a"
                           :dataset "dataset-a"
                           :dtype "host"
                           :mutation-kind "create"
                           :accepted-at accepted-at))
                (blob-2 (star-git:write-blob
                         repository
                         (star-git-test-octets "{\"value\":2}")))
                (commit-2 (star-git:commit-document
                           repository
                           "doc-cas"
                           blob-2
                           :tenant "tenant-a"
                           :dataset "dataset-a"
                           :dtype "host"
                           :mutation-kind "update"
                           :accepted-at (1+ accepted-at)))
                (ref (star-git:document-ref-name
                      "doc-cas"
                      :tenant "tenant-a"
                      :dataset "dataset-a")))
           (signals star-git:ref-conflict
             (star-git:update-ref repository ref commit-1 :expected commit-1))
           (is (string= commit-2 (star-git:resolve-ref repository ref))))
      (star-git:close-repository repository))))

(test star-git-pack-is-deterministic-and-restores-history
  (let ((source
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-pack-source/"))
        (restored
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-pack-restored/"))
        (pack-a #P"/tmp/test-tek9-star-git-pack-source/archive/a.sgp.lzma")
        (pack-b #P"/tmp/test-tek9-star-git-pack-source/archive/b.sgp.lzma"))
    (unwind-protect
         (let* ((accepted-at (encode-universal-time 0 0 0 17 9 2026 0))
                (blob-1 (star-git:write-blob
                         source
                         (star-git-test-octets "{\"packed\":1}")))
                (commit-1 (star-git:commit-document
                           source "doc-pack" blob-1
                           :tenant "tenant-a"
                           :dataset "dataset-a"
                           :dtype "person"
                           :mutation-kind "create"
                           :accepted-at accepted-at))
                (blob-2 (star-git:write-blob
                         source
                         (star-git-test-octets "{\"packed\":2}")))
                (commit-2 (star-git:commit-document
                           source "doc-pack" blob-2
                           :tenant "tenant-a"
                           :dataset "dataset-a"
                           :dtype "person"
                           :mutation-kind "update"
                           :accepted-at (1+ accepted-at)))
                (objects (list commit-2 blob-1 commit-1 blob-2 blob-1)))
           (multiple-value-bind (pack-id-a metadata-a)
               (star-git:build-pack source objects pack-a)
             (multiple-value-bind (pack-id-b metadata-b)
                 (star-git:build-pack source objects pack-b)
               (declare (ignore metadata-b))
               (is (string= pack-id-a pack-id-b))
               (is (equalp (star-git-read-file-octets pack-a)
                           (star-git-read-file-octets pack-b)))
               (is (= 4 (getf metadata-a :object-count)))
               (multiple-value-bind (imported-id imported-metadata)
                   (star-git:import-pack restored pack-a)
                 (declare (ignore imported-metadata))
                 (is (string= pack-id-a imported-id))
                 (is (equalp (star-git-test-octets "{\"packed\":1}")
                             (star-git:read-blob restored blob-1)))
                 (let ((history (star-git:log-document restored "doc-pack")))
                   (is (= 2 (length history)))
                   (is (equal nil (getf (first history) :parents)))
                   (is (equal (list commit-1)
                              (getf (second history) :parents))))
                 (let ((locations (star-git:pack-locations restored blob-2)))
                   (is (= 1 (length locations)))
                   (is (string= pack-id-a
                                (getf (first locations) :pack-id))))
                 (is (null (star-git:fsck restored))))))))
      (star-git:close-repository source)
      (star-git:close-repository restored))))

(test star-git-pack-import-rejects-corruption
  (let ((source
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-corrupt-source/"))
        (restored
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-corrupt-restored/"))
        (pack #P"/tmp/test-tek9-star-git-corrupt-source/archive/good.sgp.lzma")
        (corrupt #P"/tmp/test-tek9-star-git-corrupt-source/archive/bad.sgp.lzma"))
    (unwind-protect
         (let ((blob (star-git:write-blob
                      source
                      (star-git-test-octets "{\"integrity\":true}"))))
           (star-git:build-pack source (list blob) pack)
           (uiop:copy-file pack corrupt)
           (with-open-file (stream corrupt
                                   :direction :io
                                   :element-type '(unsigned-byte 8))
             (let ((position (1- (file-length stream))))
               (file-position stream position)
               (let ((byte (read-byte stream)))
                 (file-position stream position)
                 (write-byte (logxor byte 1) stream))))
           (signals star-git:lzma-codec-error
             (star-git:import-pack restored corrupt)))
      (star-git:close-repository source)
      (star-git:close-repository restored))))
