(in-package :tek9-tests)
(in-suite :tek9-tests)

(test ingest-worker-applies-bounded-json-batch
  (let ((db (setup-db #P"/tmp/test-tek9-ingest-worker/")))
    (unwind-protect
         (let* ((request
                  "{\"op\":\"apply_batch\",\"source_id\":\"navidrome\",\"generation\":1,\"expected_generation\":0,\"graph_name\":\"music\",\"watermark\":\"b-track\",\"documents\":[{\"id\":\"track:1\",\"value\":{\"dtype\":\"track\",\"title\":\"One\"}}],\"nodes\":[{\"id\":\"track:1\",\"props\":{\"dtype\":\"track\"}},{\"id\":\"artist:1\",\"props\":{\"dtype\":\"artist\"}}],\"edges\":[{\"id\":\"edge:1\",\"source\":\"track:1\",\"predicate\":\"performed-by\",\"target\":\"artist:1\"}]}")
                (response (apply-ingest-json-request db request))
                (parsed (jsown:parse response)))
           (is (eq t (jsown:val parsed "ok")))
           (is (= 1 (jsown:val parsed "generation")))
           (is (string= "One"
                        (jsown:val (fetch* db "track:1") "title")))
           (is (equal '("artist:1")
                      (mapcar #'node-id
                              (fetch-node-neighbors
                               db "track:1"
                               :database-name "music"
                               :predicate "performed-by")))))
      (close-database db))))

(test ingest-worker-status-reports-committed-generation
  (let ((db (setup-db #P"/tmp/test-tek9-ingest-worker-status/")))
    (unwind-protect
         (progn
           (apply-ingest-batch db "navidrome" 7
                               :expected-generation 0
                               :watermark "row-7")
           (let* ((response
                    (apply-ingest-json-request
                     db
                     "{\"op\":\"status\",\"source_id\":\"navidrome\"}"))
                  (parsed (jsown:parse response)))
             (is (eq t (jsown:val parsed "ok")))
             (is (= 7 (jsown:val parsed "generation")))
             (is (string= "row-7" (jsown:val parsed "watermark")))))
      (close-database db))))

(test ingest-worker-rejects-unknown-operation-without-evaluation
  (let ((db (setup-db #P"/tmp/test-tek9-ingest-worker-unknown/")))
    (unwind-protect
         (let* ((response
                  (apply-ingest-json-request
                   db
                   "{\"op\":\"eval\",\"form\":\"(delete-file \\\"/tmp/nope\\\")\"}"))
                (parsed (jsown:parse response)))
           (is (eq nil (jsown:val parsed "ok")))
           (is (string= "unsupported_operation"
                        (jsown:val parsed "code"))))
      (close-database db))))

(test ingest-worker-generation-conflict-is-structured
  (let ((db (setup-db #P"/tmp/test-tek9-ingest-worker-conflict/")))
    (unwind-protect
         (progn
           (apply-ingest-batch db "navidrome" 2 :expected-generation 0)
           (let* ((response
                    (apply-ingest-json-request
                     db
                     "{\"op\":\"apply_batch\",\"source_id\":\"navidrome\",\"generation\":3,\"expected_generation\":0,\"documents\":[],\"nodes\":[],\"edges\":[]}"))
                  (parsed (jsown:parse response)))
             (is (eq nil (jsown:val parsed "ok")))
             (is (string= "generation_conflict"
                          (jsown:val parsed "code")))
             (is (= 2 (jsown:val parsed "actual_generation")))))
      (close-database db))))
