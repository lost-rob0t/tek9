(in-package :tek9-tests)
(in-suite :tek9-tests)

(test indexed-graph-adjacency
  (let ((db (setup-db #P"/tmp/test-tek9-graph/"))
        (graph-name "graph-test"))
    (unwind-protect
         (let ((a (make-instance 'node :id "a" :props '(:name "A")))
               (b (make-instance 'node :id "b" :props '(:name "B")))
               (c (make-instance 'node :id "c" :props '(:name "C"))))
           (put-nodes db (list a b c) :database-name graph-name :sorted t)
           (put-edges db
                      (list (make-instance 'edge
                                           :id "e1"
                                           :source "a"
                                           :predicate "knows"
                                           :target "b")
                            (make-instance 'edge
                                           :id "e2"
                                           :source "a"
                                           :predicate "knows"
                                           :target "c")
                            (make-instance 'edge
                                           :id "e3"
                                           :source "a"
                                           :predicate "works-with"
                                           :target "c"))
                      :database-name graph-name)
           (is (= 3 (length (fetch-node-edge-ids db "a"
                                                 :database-name graph-name))))
           (is (equal '("e1" "e2")
                      (fetch-node-edge-ids db "a"
                                           :database-name graph-name
                                           :predicate "knows")))
           (is (equal '("b" "c")
                      (sort (mapcar #'node-id
                                    (fetch-node-neighbors
                                     db "a"
                                     :database-name graph-name
                                     :predicate "knows"))
                            #'string<)))
           (is (equal '("a")
                      (mapcar #'node-id
                              (fetch-node-neighbors
                               db "c"
                               :database-name graph-name
                               :incoming t
                               :predicate "works-with")))))
      (close-database db))))

(test parallel-graph-edges
  (let ((db (setup-db #P"/tmp/test-tek9-parallel-edges/"))
        (graph-name "parallel"))
    (unwind-protect
         (progn
           (put-nodes db
                      (list (make-instance 'node :id "a")
                            (make-instance 'node :id "b"))
                      :database-name graph-name
                      :sorted t)
           (let ((first (make-instance 'edge
                                       :id "edge-1"
                                       :source "a"
                                       :predicate "knows"
                                       :target "b"))
                 (second (make-instance 'edge
                                        :id "edge-2"
                                        :source "a"
                                        :predicate "knows"
                                        :target "b")))
             (put-edges db (list first second) :database-name graph-name)
             (is (equal '("edge-1" "edge-2")
                        (fetch-node-edge-ids db "a"
                                             :database-name graph-name
                                             :predicate "knows")))
             (is (= 2 (length (fetch-node-edges db "a"
                                                 :database-name graph-name
                                                 :predicate "knows"))))
             (delete-edge db first :database-name graph-name)
             (is (equal '("edge-2")
                        (fetch-node-edge-ids db "a"
                                             :database-name graph-name
                                             :predicate "knows")))))
      (close-database db))))

(test replacing-edge-rewrites-adjacency
  (let ((db (setup-db #P"/tmp/test-tek9-edge-replace/"))
        (graph-name "replace"))
    (unwind-protect
         (progn
           (put-nodes db
                      (list (make-instance 'node :id "a")
                            (make-instance 'node :id "b")
                            (make-instance 'node :id "c"))
                      :database-name graph-name)
           (put-edge db
                     (make-instance 'edge
                                    :id "edge"
                                    :source "a"
                                    :predicate "knows"
                                    :target "b")
                     :database-name graph-name)
           (is (equal '("b")
                      (mapcar #'node-id
                              (fetch-node-neighbors db "a"
                                                    :database-name graph-name
                                                    :predicate "knows"))))
           (put-edge db
                     (make-instance 'edge
                                    :id "edge"
                                    :source "a"
                                    :predicate "works-with"
                                    :target "c")
                     :database-name graph-name)
           (is (null (fetch-node-neighbors db "a"
                                            :database-name graph-name
                                            :predicate "knows")))
           (is (equal '("c")
                      (mapcar #'node-id
                              (fetch-node-neighbors db "a"
                                                    :database-name graph-name
                                                    :predicate "works-with"))))
           (is (null (fetch-node-neighbors db "b"
                                            :database-name graph-name
                                            :incoming t
                                            :predicate "knows"))))
      (close-database db))))

(test graph-namespaces-isolate-external-ids
  (let ((db (setup-db #P"/tmp/test-tek9-graph-namespaces/")))
    (unwind-protect
         (progn
           (put-nodes db
                      (list (make-instance 'node :id "same" :props '(:graph "one"))
                            (make-instance 'node :id "target" :props '(:graph "one")))
                      :database-name "graph-one")
           (put-nodes db
                      (list (make-instance 'node :id "same" :props '(:graph "two"))
                            (make-instance 'node :id "target" :props '(:graph "two")))
                      :database-name "graph-two")
           (put-edge db
                     (make-instance 'edge
                                    :id "edge"
                                    :source "same"
                                    :predicate "knows"
                                    :target "target")
                     :database-name "graph-one")
           (is (equal "one"
                      (getf (node-props (fetch-node db "same"
                                                   :database-name "graph-one"))
                            :graph)))
           (is (equal "two"
                      (getf (node-props (fetch-node db "same"
                                                   :database-name "graph-two"))
                            :graph)))
           (is (= 1 (length (fetch-node-neighbors db "same"
                                                  :database-name "graph-one"
                                                  :predicate "knows"))))
           (is (null (fetch-node-neighbors db "same"
                                            :database-name "graph-two"
                                            :predicate "knows"))))
      (close-database db))))
