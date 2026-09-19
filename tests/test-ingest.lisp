(in-package :tek9-tests)
(in-suite :tek9-tests)

(test ingest-batch-commits-records-and-checkpoint-atomically
  (let ((db (setup-db #P"/tmp/test-tek9-ingest-batch/")))
    (unwind-protect
         (progn
           (apply-ingest-batch
            db
            "navidrome"
            1
            :expected-generation 0
            :documents
            (list (new-document :id "track:1"
                                :value '(:dtype "track" :title "One")))
            :nodes
            (list (make-instance 'node
                                 :id "track:1"
                                 :props '(:dtype "track" :title "One"))
                  (make-instance 'node
                                 :id "artist:1"
                                 :props '(:dtype "artist" :name "Artist")))
            :edges
            (list (make-instance 'edge
                                 :id "edge:track-artist"
                                 :source "track:1"
                                 :predicate "performed-by"
                                 :target "artist:1"))
            :graph-name "music"
            :watermark "b-track")
           (is (string= "One" (getf (fetch* db "track:1") :title)))
           (is (string= "Artist"
                        (getf (node-props
                               (fetch-node db "artist:1"
                                           :database-name "music"))
                              :name)))
           (is (equal '("artist:1")
                      (mapcar #'node-id
                              (fetch-node-neighbors
                               db "track:1"
                               :database-name "music"
                               :predicate "performed-by"))))
           (let ((checkpoint (fetch-ingest-checkpoint db "navidrome")))
             (is (= 1 (getf checkpoint :generation)))
             (is (string= "b-track" (getf checkpoint :watermark)))))
      (close-database db))))

(test ingest-batch-rolls-back-checkpoint-and-documents-on-graph-failure
  (let ((db (setup-db #P"/tmp/test-tek9-ingest-rollback/")))
    (unwind-protect
         (progn
           (apply-ingest-batch
            db "navidrome" 1
            :expected-generation 0
            :documents
            (list (new-document :id "baseline"
                                :value '(:dtype "track")))
            :nodes
            (list (make-instance 'node :id "known"))
            :graph-name "music")
           (signals error
             (apply-ingest-batch
              db "navidrome" 2
              :expected-generation 1
              :documents
              (list (new-document :id "must-rollback"
                                  :value '(:dtype "track")))
              :edges
              (list (make-instance 'edge
                                   :id "bad-edge"
                                   :source "known"
                                   :predicate "related"
                                   :target "missing"))
              :graph-name "music"
              :watermark "later"))
           (is (null (fetch db "must-rollback")))
           (let ((checkpoint (fetch-ingest-checkpoint db "navidrome")))
             (is (= 1 (getf checkpoint :generation)))
             (is (null (getf checkpoint :watermark)))))
      (close-database db))))

(test ingest-batch-generation-fence-rejects-stale-writer
  (let ((db (setup-db #P"/tmp/test-tek9-ingest-fence/")))
    (unwind-protect
         (progn
           (apply-ingest-batch db "navidrome" 1 :expected-generation 0)
           (signals ingest-generation-conflict
             (apply-ingest-batch
              db "navidrome" 2
              :expected-generation 0
              :documents
              (list (new-document :id "stale" :value 1))))
           (is (null (fetch db "stale")))
           (is (= 1
                  (getf (fetch-ingest-checkpoint db "navidrome")
                        :generation))))
      (close-database db))))

(test ingest-batch-rejects-non-monotonic-generation
  (let ((db (setup-db #P"/tmp/test-tek9-ingest-monotonic/")))
    (unwind-protect
         (progn
           (apply-ingest-batch db "navidrome" 3 :expected-generation 0)
           (signals ingest-generation-conflict
             (apply-ingest-batch db "navidrome" 3 :expected-generation 3))
           (signals ingest-generation-conflict
             (apply-ingest-batch db "navidrome" 2 :expected-generation 3))
           (is (= 3
                  (getf (fetch-ingest-checkpoint db "navidrome")
                        :generation))))
      (close-database db))))
