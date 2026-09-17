(in-package :tek9-tests)

(in-suite :tek9-tests)

(defun star-git-v2-provenance (&rest overrides)
  (let ((base
          (list :couch-rev "2-example"
                :source-actor "dns-resolver"
                :operation-id "op-1"
                :trace-id "trace-1"
                :refresh-generation 4
                :policy-digest "sha256:policy"
                :content-digest "sha256:content"
                :schema-release "0.9.1"
                :schema-version "0.9.0")))
    (loop for (key value) on overrides by #'cddr
          do (setf (getf base key) value))
    base))

(test star-git-commit-v2-normalizes-provenance-order
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-v2-order/")))
    (unwind-protect
         (let* ((accepted-at (encode-universal-time 0 0 0 17 9 2026 0))
                (blob-id (star-git:write-blob
                          repository
                          (star-git-test-octets "{\"v2\":true}")))
                (provenance-a (star-git-v2-provenance))
                (provenance-b
                  (list :schema-version "0.9.0"
                        :policy-digest "sha256:policy"
                        :couch-rev "2-example"
                        :trace-id "trace-1"
                        :refresh-generation 4
                        :schema-release "0.9.1"
                        :content-digest "sha256:content"
                        :operation-id "op-1"
                        :source-actor "dns-resolver"))
                (commit-a
                  (star-git:commit-document
                   repository "doc-v2" blob-id
                   :tenant "tenant-a"
                   :dataset "dataset-a"
                   :dtype "host"
                   :mutation-kind "update"
                   :accepted-at accepted-at
                   :parents nil
                   :update-ref-p nil
                   :format-version 2
                   :provenance provenance-a))
                (commit-b
                  (star-git:commit-document
                   repository "doc-v2" blob-id
                   :tenant "tenant-a"
                   :dataset "dataset-a"
                   :dtype "host"
                   :mutation-kind "update"
                   :accepted-at accepted-at
                   :parents nil
                   :update-ref-p nil
                   :format-version 2
                   :provenance provenance-b))
                (record (star-git:read-commit repository commit-a)))
           (is (string= commit-a commit-b))
           (is (= 2 (star-git:commit-format-version record)))
           (is (equal (star-git:normalize-commit-provenance provenance-a)
                      (star-git:commit-provenance record)))
           (is (string=
                commit-a
                (getf (star-git:find-commit-by-couch-rev
                       repository "doc-v2" "2-example"
                       :tenant "tenant-a"
                       :dataset "dataset-a")
                      :commit-id)))
           (signals star-git:invalid-provenance
             (star-git:commit-document
              repository "doc-v2" blob-id
              :tenant "tenant-a"
              :dataset "dataset-a"
              :dtype "host"
              :accepted-at accepted-at
              :parents nil
              :update-ref-p nil
              :format-version 2
              :provenance '(:authorization "Bearer nope"))))
      (star-git:close-repository repository))))

(test star-git-commit-v2-binds-material-provenance
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-v2-material/")))
    (unwind-protect
         (let* ((accepted-at (encode-universal-time 0 0 0 17 9 2026 0))
                (blob-id (star-git:write-blob
                          repository
                          (star-git-test-octets "{\"same\":\"blob\"}")))
                (base-args
                  (list :tenant "tenant-a"
                        :dataset "dataset-a"
                        :dtype "host"
                        :mutation-kind "update"
                        :accepted-at accepted-at
                        :parents nil
                        :update-ref-p nil
                        :format-version 2))
                (base
                  (apply #'star-git:commit-document
                         repository "doc-material" blob-id
                         (append base-args
                                 (list :provenance
                                       (star-git-v2-provenance)))))
                (different-rev
                  (apply #'star-git:commit-document
                         repository "doc-material" blob-id
                         (append base-args
                                 (list :provenance
                                       (star-git-v2-provenance
                                        :couch-rev "3-example")))))
                (different-generation
                  (apply #'star-git:commit-document
                         repository "doc-material" blob-id
                         (append base-args
                                 (list :provenance
                                       (star-git-v2-provenance
                                        :refresh-generation 5)))))
                (different-policy
                  (apply #'star-git:commit-document
                         repository "doc-material" blob-id
                         (append base-args
                                 (list :provenance
                                       (star-git-v2-provenance
                                        :policy-digest "sha256:other"))))))
           (is (not (string= base different-rev)))
           (is (not (string= base different-generation)))
           (is (not (string= base different-policy)))
           (let ((explicit-nils
                   (star-git:commit-document
                    repository "doc-nil" blob-id
                    :tenant "tenant-a"
                    :dataset "dataset-a"
                    :dtype "host"
                    :accepted-at accepted-at
                    :parents nil
                    :update-ref-p nil
                    :format-version 2
                    :provenance '(:couch-rev nil)))
                 (omitted
                   (star-git:commit-document
                    repository "doc-nil" blob-id
                    :tenant "tenant-a"
                    :dataset "dataset-a"
                    :dtype "host"
                    :accepted-at accepted-at
                    :parents nil
                    :update-ref-p nil
                    :format-version 2)))
             (is (string= explicit-nils omitted))))
      (star-git:close-repository repository))))

(test star-git-couch-revision-index-is-document-scoped
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-v2-scope/")))
    (unwind-protect
         (let* ((accepted-at (encode-universal-time 0 0 0 17 9 2026 0))
                (blob-a (star-git:write-blob
                         repository
                         (star-git-test-octets "{\"scope\":\"a\"}")))
                (blob-b (star-git:write-blob
                         repository
                         (star-git-test-octets "{\"scope\":\"b\"}")))
                (commit-a
                  (star-git:commit-document
                   repository "doc-a" blob-a
                   :tenant "tenant-a"
                   :dataset "dataset-a"
                   :dtype "person"
                   :accepted-at accepted-at
                   :format-version 2
                   :provenance (star-git-v2-provenance
                                :couch-rev "1-same")))
                (commit-b
                  (star-git:commit-document
                   repository "doc-b" blob-b
                   :tenant "tenant-a"
                   :dataset "dataset-a"
                   :dtype "person"
                   :accepted-at accepted-at
                   :format-version 2
                   :provenance (star-git-v2-provenance
                                :couch-rev "1-same"))))
           (is (string=
                commit-a
                (getf (star-git:find-commit-by-couch-rev
                       repository "doc-a" "1-same"
                       :tenant "tenant-a"
                       :dataset "dataset-a")
                      :commit-id)))
           (is (string=
                commit-b
                (getf (star-git:find-commit-by-couch-rev
                       repository "doc-b" "1-same"
                       :tenant "tenant-a"
                       :dataset "dataset-a")
                      :commit-id)))
           (is (null (star-git:find-commit-by-couch-rev
                      repository "doc-a" "1-same"
                      :tenant "tenant-b"
                      :dataset "dataset-a"))))
      (star-git:close-repository repository))))

(test star-git-history-can-mix-commit-v1-and-v2
  (let ((repository
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-v2-mixed/")))
    (unwind-protect
         (let* ((accepted-at (encode-universal-time 0 0 0 17 9 2026 0))
                (blob-1 (star-git:write-blob
                         repository
                         (star-git-test-octets "{\"mixed\":1}")))
                (commit-1
                  (star-git:commit-document
                   repository "doc-mixed" blob-1
                   :tenant "tenant-a"
                   :dataset "dataset-a"
                   :dtype "person"
                   :mutation-kind "create"
                   :accepted-at accepted-at))
                (blob-2 (star-git:write-blob
                         repository
                         (star-git-test-octets "{\"mixed\":2}")))
                (commit-2
                  (star-git:commit-document
                   repository "doc-mixed" blob-2
                   :tenant "tenant-a"
                   :dataset "dataset-a"
                   :dtype "person"
                   :accepted-at (1+ accepted-at)
                   :format-version 2
                   :provenance (star-git-v2-provenance
                                :couch-rev "2-mixed")))
                (blob-3 (star-git:write-blob
                         repository
                         (star-git-test-octets "{\"mixed\":3}")))
                (commit-3
                  (star-git:commit-document
                   repository "doc-mixed" blob-3
                   :tenant "tenant-a"
                   :dataset "dataset-a"
                   :dtype "person"
                   :accepted-at (+ 2 accepted-at)
                   :format-version 1))
                (history (star-git:log-document repository "doc-mixed")))
           (is (= 1 (star-git:commit-format-version
                     (star-git:read-commit repository commit-1))))
           (is (= 2 (star-git:commit-format-version
                     (star-git:read-commit repository commit-2))))
           (is (= 1 (star-git:commit-format-version
                     (star-git:read-commit repository commit-3))))
           (is (equal (list commit-1)
                      (getf (star-git:read-commit repository commit-2)
                            :parents)))
           (is (equal (list commit-2)
                      (getf (star-git:read-commit repository commit-3)
                            :parents)))
           (is (= 3 (length history)))
           (is (null (star-git:fsck repository))))
      (star-git:close-repository repository))))

(test star-git-pack-roundtrip-preserves-commit-v2-provenance
  (let ((source
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-v2-pack-source/"))
        (restored
          (setup-star-git-repository #P"/tmp/test-tek9-star-git-v2-pack-restored/"))
        (pack #P"/tmp/test-tek9-star-git-v2-pack-source/archive/v2.sgp.lzma"))
    (unwind-protect
         (let* ((accepted-at (encode-universal-time 0 0 0 17 9 2026 0))
                (blob-id (star-git:write-blob
                          source
                          (star-git-test-octets "{\"pack-v2\":true}")))
                (provenance
                  (star-git-v2-provenance :couch-rev "7-pack"))
                (commit-id
                  (star-git:commit-document
                   source "doc-pack-v2" blob-id
                   :tenant "tenant-a"
                   :dataset "dataset-a"
                   :dtype "host"
                   :mutation-kind "refresh"
                   :accepted-at accepted-at
                   :format-version 2
                   :provenance provenance)))
           (star-git:build-pack source (list blob-id commit-id) pack)
           (star-git:import-pack restored pack)
           (let ((record (star-git:read-commit restored commit-id)))
             (is (= 2 (star-git:commit-format-version record)))
             (is (equal (star-git:normalize-commit-provenance provenance)
                        (star-git:commit-provenance record))))
           (is (string=
                commit-id
                (getf (star-git:find-commit-by-couch-rev
                       restored "doc-pack-v2" "7-pack"
                       :tenant "tenant-a"
                       :dataset "dataset-a")
                      :commit-id)))
           (is (null (star-git:fsck restored))))
      (star-git:close-repository source)
      (star-git:close-repository restored))))
