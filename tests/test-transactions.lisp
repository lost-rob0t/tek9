(in-package :tek9-tests)
(in-suite :tek9-tests)

(test outer-write-transaction-commits-mixed-records
  (let* ((path #P"/tmp/test-tek9-outer-commit/")
         (db (setup-db path))
         (graph-name "mixed"))
    (unwind-protect
         (progn
           (register-index db
                           "dtype"
                           (lambda (document)
                             (getf (doc-value document) :dtype)))
           (with-write-transaction (db)
             (put db (new-document :id "person-1"
                                   :value '(:dtype "person" :name "Ada")))
             (put-nodes db
                        (list (make-instance 'node :id "a")
                              (make-instance 'node :id "b"))
                        :database-name graph-name)
             (put-edge db
                       (make-instance 'edge
                                      :id "e1"
                                      :source "a"
                                      :predicate "knows"
                                      :target "b")
                       :database-name graph-name)
             (put db (new-document :id "meta"
                                   :value '(:revision 1))))
           (is (equal '("person-1")
                      (index-document-ids db "dtype" "person")))
           (close-database db)
           (setf db (open-database (new-database "test" :path path)))
           (is (equal "Ada" (getf (fetch* db "person-1") :name)))
           (is (= 1 (getf (fetch* db "meta") :revision)))
           (is (equal '("e1")
                      (fetch-node-edge-ids db "a" :database-name graph-name))))
      (when (and db (db-is-open-p db))
        (close-database db)))))

(test outer-write-transaction-rolls-back-mixed-records
  (let ((db (setup-db #P"/tmp/test-tek9-outer-rollback/"))
        (graph-name "rollback"))
    (unwind-protect
         (progn
           (register-index db
                           "dtype"
                           (lambda (document)
                             (getf (doc-value document) :dtype)))
           (signals error
             (with-write-transaction (db)
               (put db (new-document :id "person-1"
                                     :value '(:dtype "person" :name "Ada")))
               (put-nodes db
                          (list (make-instance 'node :id "a")
                                (make-instance 'node :id "b"))
                          :database-name graph-name)
               (put-edge db
                         (make-instance 'edge
                                        :id "e1"
                                        :source "a"
                                        :predicate "knows"
                                        :target "b")
                         :database-name graph-name)
               (put db (new-document :id "meta" :value '(:revision 1)))
               (error "inject rollback")))
           (is (null (fetch db "person-1")))
           (is (null (fetch db "meta")))
           (is (null (index-document-ids db "dtype" "person")))
           (is (null (fetch-node db "a" :database-name graph-name)))
           (is (null (fetch-edge db "e1" :database-name graph-name)))
           (is (null (fetch-node-edge-ids db "a" :database-name graph-name))))
      (close-database db))))

(test nested-tek9-transactions-reuse-compatible-boundary
  (let ((db (setup-db #P"/tmp/test-tek9-nested-transaction/")))
    (unwind-protect
         (progn
           (with-write-transaction (db)
             (put db (new-document :id "outer" :value 1))
             (with-write-transaction (db)
               (put db (new-document :id "inner" :value 2)))
             (with-read-transaction (db)
               (is (= 2 (fetch* db "inner")))))
           (is (= 1 (fetch* db "outer")))
           (is (= 2 (fetch* db "inner")))
           (signals transaction-mode-error
             (with-read-transaction (db)
               (put db (new-document :id "illegal" :value 3))))
           (is (null (fetch db "illegal"))))
      (close-database db))))

(test outer-transaction-preopens-first-use-document-databases
  (let* ((path #P"/tmp/test-tek9-first-use-dbis/")
         (db (setup-db path)))
    (unwind-protect
         (progn
           (with-write-transaction
               (db :database-names '("metadata" "journal"))
             (put db (new-document :id "meta" :value '(:revision 7))
                  :database-name "metadata")
             (put db (new-document :id "entry" :value '(:operation "commit"))
                  :database-name "journal"))
           (close-database db)
           (setf db (open-database (new-database "test" :path path)))
           (is (= 7 (getf (fetch* db "meta" :database-name "metadata") :revision)))
           (is (string= "commit"
                        (getf (fetch* db "entry" :database-name "journal")
                              :operation))))
      (when (and db (db-is-open-p db))
        (close-database db)))))

(test edge-replacement-rolls-back-with-outer-transaction
  (let ((db (setup-db #P"/tmp/test-tek9-edge-replace-rollback/"))
        (graph-name "replace-rollback"))
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
           (signals error
             (with-write-transaction (db)
               (put-edge db
                         (make-instance 'edge
                                        :id "edge"
                                        :source "a"
                                        :predicate "works-with"
                                        :target "c")
                         :database-name graph-name)
               (error "inject rollback")))
           (let ((edge (fetch-edge db "edge" :database-name graph-name)))
             (is (string= "edge" (edge-id edge)))
             (is (string= "b" (edge-target edge)))
             (is (string= "knows" (edge-predicate edge))))
           (is (equal '("b")
                      (mapcar #'node-id
                              (fetch-node-neighbors db "a"
                                                    :database-name graph-name
                                                    :predicate "knows"))))
           (is (null (fetch-node-neighbors db "a"
                                            :database-name graph-name
                                            :predicate "works-with"))))
      (close-database db))))
