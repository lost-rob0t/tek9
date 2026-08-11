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
