(in-package :tek9-tests)
(in-suite :tek9-tests)

(test reopen-persists-documents-and-indexes
  (let* ((path #P"/tmp/test-tek9-persistence/")
         (db (setup-db path)))
    (unwind-protect
         (progn
           (register-index db
                           "dtype"
                           (lambda (document)
                             (getf (doc-value document) :dtype)))
           (put db (new-document :id "p1"
                                 :value '(:dtype "person" :name "Ada")))
           (close-database db)

           ;; Recreate the Lisp handle to prove data lives in LMDB, not process state.
           (setf db (open-database (new-database "test" :path path)))
           (register-index db
                           "dtype"
                           (lambda (document)
                             (getf (doc-value document) :dtype)))

           (is (equal '(:dtype "person" :name "Ada")
                      (fetch* db "p1")))
           (is (equal '("p1")
                      (index-document-ids db "dtype" "person"))))
      (close-database db))))
