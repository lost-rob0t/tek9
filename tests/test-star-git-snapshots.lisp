(in-package :tek9-tests)

(in-suite :tek9-tests)

(test star-git-tree-is-order-independent-and-rejects-duplicates
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-tree-order/")))
    (unwind-protect
         (let* ((accepted-at (encode-universal-time 0 0 0 17 9 2026 0))
                (blob-a (star-git:write-blob
                         repository
                         (star-git-test-octets "{\"doc\":\"a\"}")))
                (commit-a (star-git:commit-document
                           repository "doc-a" blob-a
                           :tenant "tenant-a"
                           :dataset "dataset-a"
                           :dtype "person"
                           :mutation-kind "create"
                           :accepted-at accepted-at))
                (blob-b (star-git:write-blob
                         repository
                         (star-git-test-octets "{\"doc\":\"b\"}")))
                (commit-b (star-git:commit-document
                           repository "doc-b" blob-b
                           :tenant "tenant-a"
                           :dataset "dataset-a"
                           :dtype "person"
                           :mutation-kind "create"
                           :accepted-at (1+ accepted-at)))
                (ref-a (star-git:document-ref-name
                        "doc-a" :tenant "tenant-a" :dataset "dataset-a"))
                (ref-b (star-git:document-ref-name
                        "doc-b" :tenant "tenant-a" :dataset "dataset-a"))
                (tree-a (star-git:write-tree
                         repository
                         (list (cons ref-b commit-b)
                               (cons ref-a commit-a))))
                (tree-b (star-git:write-tree
                         repository
                         (list (cons ref-a commit-a)
                               (cons ref-b commit-b)))))
           (is (string= tree-a tree-b))
           (is (equal (list (cons ref-a commit-a)
                            (cons ref-b commit-b))
                      (star-git:read-tree repository tree-a)))
           (signals star-git:duplicate-tree-ref
             (star-git:write-tree
              repository
              (list (cons ref-a commit-a)
                    (cons ref-a commit-b))))
           (signals star-git:missing-object
             (star-git:write-tree
              repository
              (list (cons "refs/docs/missing" "sha256:not-a-commit")))))
      (star-git:close-repository repository))))

(test star-git-dataset-snapshot-and-tag-use-cas-refs
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-snapshot-ref/")))
    (unwind-protect
         (let* ((accepted-at (encode-universal-time 0 0 0 17 9 2026 0))
                (blob (star-git:write-blob
                       repository
                       (star-git-test-octets "{\"snapshot\":true}")))
                (commit-id (star-git:commit-document
                            repository "doc-snapshot" blob
                            :tenant "tenant-a"
                            :dataset "dataset-a"
                            :dtype "host"
                            :mutation-kind "create"
                            :accepted-at accepted-at))
                (doc-ref (star-git:document-ref-name
                          "doc-snapshot"
                          :tenant "tenant-a"
                          :dataset "dataset-a")))
           (multiple-value-bind (tree-id snapshot-ref)
               (star-git:snapshot-dataset-day
                repository
                "tenant-a"
                "dataset-a"
                "2026-09-17"
                (list (cons doc-ref commit-id))
                :expected nil)
             (is (string= tree-id
                          (star-git:resolve-ref repository snapshot-ref)))
             (is (string= snapshot-ref
                          (star-git:dataset-snapshot-ref-name
                           "tenant-a" "dataset-a" "2026-09-17")))
             (signals star-git:ref-conflict
               (star-git:snapshot-dataset-day
                repository
                "tenant-a"
                "dataset-a"
                "2026-09-17"
                (list (cons doc-ref commit-id))
                :expected nil))
             (multiple-value-bind (tag-id tag-ref)
                 (star-git:write-tag
                  repository
                  "nightly"
                  tree-id
                  :created-at accepted-at
                  :note "daily checkpoint"
                  :expected nil)
               (is (string= tag-id
                            (star-git:resolve-ref repository tag-ref)))
               (is (equal (list :name "nightly"
                                :tree-id tree-id
                                :created-at accepted-at
                                :note "daily checkpoint")
                          (star-git:read-tag repository tag-id))))))
      (star-git:close-repository repository))))

(test star-git-pack-import-does-not-guess-mutable-refs
  (let ((source
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-tree-pack-source/"))
        (restored
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-tree-pack-restored/"))
        (pack #P"/tmp/test-tek9-star-git-tree-pack-source/archive/snapshot.sgp.lzma"))
    (unwind-protect
         (let* ((accepted-at (encode-universal-time 0 0 0 17 9 2026 0))
                (blob (star-git:write-blob
                       source
                       (star-git-test-octets "{\"restore\":1}")))
                (commit-id (star-git:commit-document
                            source "doc-restore" blob
                            :tenant "tenant-a"
                            :dataset "dataset-a"
                            :dtype "person"
                            :mutation-kind "create"
                            :accepted-at accepted-at))
                (doc-ref (star-git:document-ref-name
                          "doc-restore"
                          :tenant "tenant-a"
                          :dataset "dataset-a"))
                (tree-id (star-git:write-tree
                          source
                          (list (cons doc-ref commit-id)))))
           (star-git:build-pack source
                                (list blob commit-id tree-id)
                                pack)
           (star-git:import-pack restored pack)

           ;; Pack import admits immutable history only. It must not guess heads.
           (is (null (star-git:resolve-ref restored doc-ref)))
           (is (equal (list (cons doc-ref commit-id))
                      (star-git:read-tree restored tree-id)))

           ;; Explicit restore is the only operation that advances a document ref.
           (is (string= commit-id
                        (star-git:restore-ref-from-tree
                         restored tree-id doc-ref :expected nil)))
           (is (string= commit-id
                        (star-git:resolve-ref restored doc-ref)))

           ;; A stale restore writer cannot overwrite a newer head.
           (let* ((blob-2 (star-git:write-blob
                           restored
                           (star-git-test-octets "{\"restore\":2}")))
                  (commit-2 (star-git:commit-document
                             restored "doc-restore" blob-2
                             :tenant "tenant-a"
                             :dataset "dataset-a"
                             :dtype "person"
                             :mutation-kind "update"
                             :accepted-at (1+ accepted-at))))
             (declare (ignore commit-2))
             (signals star-git:ref-conflict
               (star-git:restore-ref-from-tree
                restored tree-id doc-ref :expected commit-id))))
      (star-git:close-repository source)
      (star-git:close-repository restored))))
