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
                                           :source "a"
                                           :predicate "knows"
                                           :target "b")
                            (make-instance 'edge
                                           :source "a"
                                           :predicate "knows"
                                           :target "c"))
                      :database-name graph-name)
           (is (= 2 (length (fetch-node-edge-ids db "a"
                                                 :database-name graph-name))))
           (is (equal '("b" "c")
                      (mapcar #'node-id
                              (fetch-node-neighbors db "a"
                                                    :database-name graph-name)))))
      (close-database db))))
