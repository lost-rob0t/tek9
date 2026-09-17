(in-package :tek9-tests)

(in-suite :tek9-tests)

(defun star-git-build-blob-pack (repository count pathname)
  (let ((object-ids
          (loop for index below count
                collect
                (star-git:write-blob
                 repository
                 (star-git-test-octets
                  (format nil "{\"batch\":~D,\"payload\":\"xxxxxxxx\"}"
                          index))))))
    (star-git:build-pack repository object-ids pathname)
    object-ids))

(test star-git-pack-import-groups-object-transactions
  (let ((source
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-batch-source/"))
        (restored
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-batch-restored/"))
        (pack #P"/tmp/test-tek9-star-git-batch-source/archive/batch.sgp.lzma"))
    (unwind-protect
         (let* ((object-ids (star-git-build-blob-pack source 7 pack))
                (events nil)
                (star-git::*import-pack-batch-observer*
                  (lambda (phase count bytes)
                    (push (list phase count bytes) events))))
           (star-git:import-pack restored
                                 pack
                                 :batch-object-limit 3
                                 :batch-byte-limit (* 1024 1024))
           (let ((object-events
                   (remove-if-not
                    (lambda (event) (eq (first event) :objects))
                    (nreverse events))))
             (is (equal '(3 3 1) (mapcar #'second object-events))))
           (dolist (object-id object-ids)
             (is (equalp (star-git:read-blob source object-id)
                         (star-git:read-blob restored object-id))))
           (is (null (star-git:fsck restored))))
      (star-git:close-repository source)
      (star-git:close-repository restored))))

(test star-git-pack-import-byte-limit-splits-batches
  (let ((source
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-byte-source/"))
        (restored
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-byte-restored/"))
        (pack #P"/tmp/test-tek9-star-git-byte-source/archive/bytes.sgp.lzma"))
    (unwind-protect
         (let ((events nil))
           (star-git-build-blob-pack source 4 pack)
           (let ((star-git::*import-pack-batch-observer*
                   (lambda (phase count bytes)
                     (push (list phase count bytes) events))))
             (star-git:import-pack restored
                                   pack
                                   :batch-object-limit 100
                                   :batch-byte-limit 1))
           (let ((object-events
                   (remove-if-not
                    (lambda (event) (eq (first event) :objects))
                    (nreverse events))))
             (is (= 4 (length object-events)))
             (is (every (lambda (event) (= 1 (second event)))
                        object-events))))
      (star-git:close-repository source)
      (star-git:close-repository restored))))

(test star-git-pack-import-retry-after-committed-batch-is-idempotent
  (let ((source
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-retry-source/"))
        (restored
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-retry-restored/"))
        (pack #P"/tmp/test-tek9-star-git-retry-source/archive/retry.sgp.lzma"))
    (unwind-protect
         (let* ((object-ids (star-git-build-blob-pack source 5 pack))
                (pack-id (star-git:pack-id-for-path pack))
                (object-batches 0))
           (signals error
             (let ((star-git::*import-pack-batch-observer*
                     (lambda (phase count bytes)
                       (declare (ignore count bytes))
                       (when (eq phase :objects)
                         (incf object-batches)
                         (when (= object-batches 1)
                           (error "simulated crash after committed import batch"))))))
               (star-git:import-pack restored
                                     pack
                                     :batch-object-limit 2
                                     :batch-byte-limit (* 1024 1024))))
           (is (= 1 object-batches))
           (is (null (star-git:read-pack-metadata restored pack-id)))
           (multiple-value-bind (retry-pack-id metadata)
               (star-git:import-pack restored
                                     pack
                                     :batch-object-limit 2
                                     :batch-byte-limit (* 1024 1024))
             (declare (ignore metadata))
             (is (string= pack-id retry-pack-id)))
           (dolist (object-id object-ids)
             (is (star-git:read-object restored object-id)))
           (is (null (star-git:fsck restored))))
      (star-git:close-repository source)
      (star-git:close-repository restored))))

(test star-git-batched-import-rebuilds-commit-v2-index
  (let ((source
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-batch-v2-source/"))
        (restored
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-batch-v2-restored/"))
        (pack #P"/tmp/test-tek9-star-git-batch-v2-source/archive/v2.sgp.lzma"))
    (unwind-protect
         (let* ((accepted-at (encode-universal-time 0 0 0 17 9 2026 0))
                (blob-1 (star-git:write-blob
                         source
                         (star-git-test-octets "{\"batch-v2\":1}")))
                (commit-1
                  (star-git:commit-document
                   source "doc-batch-v2" blob-1
                   :tenant "tenant-a"
                   :dataset "dataset-a"
                   :dtype "host"
                   :mutation-kind "create"
                   :accepted-at accepted-at
                   :format-version 2
                   :provenance
                   '(:couch-rev "1-batch"
                     :source-actor "collector"
                     :operation-id "op-1"
                     :trace-id nil
                     :refresh-generation 1
                     :policy-digest "sha256:p1"
                     :content-digest "sha256:c1"
                     :schema-release "0.9.1"
                     :schema-version "0.9.0")))
                (blob-2 (star-git:write-blob
                         source
                         (star-git-test-octets "{\"batch-v2\":2}")))
                (commit-2
                  (star-git:commit-document
                   source "doc-batch-v2" blob-2
                   :tenant "tenant-a"
                   :dataset "dataset-a"
                   :dtype "host"
                   :mutation-kind "update"
                   :accepted-at (1+ accepted-at)
                   :format-version 2
                   :provenance
                   '(:couch-rev "2-batch"
                     :source-actor "collector"
                     :operation-id "op-2"
                     :trace-id nil
                     :refresh-generation 2
                     :policy-digest "sha256:p1"
                     :content-digest "sha256:c2"
                     :schema-release "0.9.1"
                     :schema-version "0.9.0"))))
           (star-git:build-pack source
                                (list blob-1 commit-1 blob-2 commit-2)
                                pack)
           (star-git:import-pack restored
                                 pack
                                 :batch-object-limit 1
                                 :batch-byte-limit (* 1024 1024))
           (is (string=
                commit-1
                (getf (star-git:find-commit-by-couch-rev
                       restored "doc-batch-v2" "1-batch"
                       :tenant "tenant-a"
                       :dataset "dataset-a")
                      :commit-id)))
           (is (string=
                commit-2
                (getf (star-git:find-commit-by-couch-rev
                       restored "doc-batch-v2" "2-batch"
                       :tenant "tenant-a"
                       :dataset "dataset-a")
                      :commit-id)))
           (is (= 2 (length (star-git:log-document restored "doc-batch-v2"))))
           (is (null (star-git:fsck restored))))
      (star-git:close-repository source)
      (star-git:close-repository restored))))
