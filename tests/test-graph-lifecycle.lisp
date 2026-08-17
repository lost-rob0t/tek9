(in-package :tek9-tests)
(in-suite :tek9-tests)

(test graph-fetch-and-enumeration
  (let ((db (setup-db #P"/tmp/test-tek9-graph-enumeration/"))
        (graph-name "enumerate"))
    (unwind-protect
         (progn
           (put-nodes db
                      (list (make-instance 'node :id "c")
                            (make-instance 'node :id "a")
                            (make-instance 'node :id "b"))
                      :database-name graph-name)
           (put-edges db
                      (list (make-instance 'edge :id "e2" :source "a"
                                                :predicate "knows" :target "c")
                            (make-instance 'edge :id "e1" :source "a"
                                                :predicate "knows" :target "b"))
                      :database-name graph-name)
           (is (equal '("a" "b" "c")
                      (mapcar #'node-id
                              (fetch-graph-nodes db graph-name))))
           (is (equal '("e1" "e2")
                      (mapcar #'edge-id
                              (fetch-graph-edges db graph-name))))
           (let ((edge (fetch-edge db "e1" :database-name graph-name)))
             (is (string= "a" (edge-source edge)))
             (is (string= "b" (edge-target edge)))))
      (close-database db))))

(test delete-node-removes-incident-edges-and-adjacency
  (let ((db (setup-db #P"/tmp/test-tek9-delete-node/"))
        (graph-name "delete-node"))
    (unwind-protect
         (progn
           (put-nodes db
                      (list (make-instance 'node :id "a")
                            (make-instance 'node :id "b")
                            (make-instance 'node :id "c"))
                      :database-name graph-name)
           (put-edges db
                      (list (make-instance 'edge :id "ab-1" :source "a"
                                                :predicate "knows" :target "b")
                            (make-instance 'edge :id "ab-2" :source "a"
                                                :predicate "works-with" :target "b")
                            (make-instance 'edge :id "bc" :source "b"
                                                :predicate "knows" :target "c")
                            (make-instance 'edge :id "ac" :source "a"
                                                :predicate "knows" :target "c"))
                      :database-name graph-name)
           (is (delete-node db "b" :database-name graph-name))
           (is (null (fetch-node db "b" :database-name graph-name)))
           (is (null (fetch-edge db "ab-1" :database-name graph-name)))
           (is (null (fetch-edge db "ab-2" :database-name graph-name)))
           (is (null (fetch-edge db "bc" :database-name graph-name)))
           (is (not (null (fetch-edge db "ac" :database-name graph-name))))
           (is (equal '("ac")
                      (fetch-node-edge-ids db "a" :database-name graph-name)))
           (is (equal '("ac")
                      (fetch-node-edge-ids db "c"
                                           :database-name graph-name
                                           :incoming t)))
           (is (null (fetch-node-neighbors db "a"
                                            :database-name graph-name
                                            :predicate "works-with"))))
      (close-database db))))

(test clear-graph-preserves-other-namespaces
  (let ((db (setup-db #P"/tmp/test-tek9-clear-graph/")))
    (unwind-protect
         (progn
           (dolist (graph-name '("one" "two"))
             (put-nodes db
                        (list (make-instance 'node :id "same")
                              (make-instance 'node :id "target"))
                        :database-name graph-name)
             (put-edge db
                       (make-instance 'edge :id "edge" :source "same"
                                            :predicate "knows" :target "target")
                       :database-name graph-name))
           (is (clear-graph db "one"))
           (is (null (fetch-graph-nodes db "one")))
           (is (null (fetch-graph-edges db "one")))
           (is (null (fetch-node db "same" :database-name "one")))
           (is (null (fetch-edge db "edge" :database-name "one")))
           (is (equal '("same" "target")
                      (mapcar #'node-id (fetch-graph-nodes db "two"))))
           (is (equal '("edge")
                      (mapcar #'edge-id (fetch-graph-edges db "two"))))
           (is (= 1 (length (fetch-node-neighbors db "same"
                                                  :database-name "two")))))
      (close-database db))))

(test graph-lifecycle-rollback-and-reopen
  (let* ((path #P"/tmp/test-tek9-graph-lifecycle-rollback/")
         (db (setup-db path))
         (graph-name "durable"))
    (unwind-protect
         (progn
           (put-nodes db
                      (list (make-instance 'node :id "a")
                            (make-instance 'node :id "b"))
                      :database-name graph-name)
           (put-edge db
                     (make-instance 'edge :id "edge" :source "a"
                                          :predicate "knows" :target "b")
                     :database-name graph-name)
           (signals error
             (with-write-transaction (db)
               (clear-graph db graph-name)
               (error "abort graph clear")))
           (is (= 2 (length (fetch-graph-nodes db graph-name))))
           (is (= 1 (length (fetch-graph-edges db graph-name))))
           (close-database db)
           (setf db (open-database (new-database "test" :path path)))
           (is (equal '("a" "b")
                      (mapcar #'node-id (fetch-graph-nodes db graph-name))))
           (is (equal '("edge")
                      (mapcar #'edge-id (fetch-graph-edges db graph-name)))))
      (when (and db (db-is-open-p db))
        (close-database db)))))
