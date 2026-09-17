(in-package :tek9-tests)

(in-suite :tek9-tests)

(defun setup-star-git-repository (path)
  (when (uiop:directory-exists-p path)
    (uiop:delete-directory-tree path :validate t))
  (star-git:open-repository path))

(defun star-git-test-octets (string)
  (babel:string-to-octets string :encoding :utf-8))

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
