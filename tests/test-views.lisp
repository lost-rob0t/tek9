(in-package :tek9-tests)
(in-suite :tek9-tests)

(test view-rebuild-and-incremental-update
  (let ((db (setup-db #P"/tmp/test-tek9-views/")))
    (unwind-protect
         (let ((view (new-view "people-by-id" nil)))
           (define-map view
             (when (string= "person" (getf (doc-value doc) :dtype))
               (emit (doc-id doc) (getf (doc-value doc) :name))))
           (add-view db view)

           (put-bulk db
                     (list (new-document :id "p1"
                                         :value '(:dtype "person" :name "Ada"))
                           (new-document :id "o1"
                                         :value '(:dtype "organization" :name "ACME"))))
           (apply-view-to-database db view)

           (let ((view-db (create-view-db db view)))
             (with-database (db)
               (is (string= "Ada" (tek9:$ (lmdb:g3t view-db "p1"))))
               (is (null (lmdb:g3t view-db "o1")))))

           ;; A rebuild must clear rows that no longer match the mapper.
           (put db (new-document :id "p1"
                                 :value '(:dtype "organization" :name "Ada")))
           (apply-view-to-database db view)
           (let ((view-db (create-view-db db view)))
             (with-database (db)
               (is (null (lmdb:g3t view-db "p1")))))

           ;; WITH-VIEWS applies touched document ids and then clears the change vector.
           (with-views db (list view)
             (put db (new-document :id "p2"
                                   :value '(:dtype "person" :name "Grace"))))
           (let ((view-db (create-view-db db view)))
             (with-database (db)
               (is (string= "Grace" (tek9:$ (lmdb:g3t view-db "p2"))))))
           (is (zerop (length (db-changed db))))

           (is (delete-view db view))
           (let ((view-db (create-view-db db view)))
             (with-database (db)
               (is (null (lmdb:g3t view-db "p2"))))))
      (close-database db))))
