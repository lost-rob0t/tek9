(in-package :tek9-tests)
(in-suite :tek9-tests)

(defun dtype-index-definition ()
  (new-index-definition
   "dtype"
   (lambda (document)
     (getf (doc-value document) :dtype))))

(defun name-index-definition ()
  (new-index-definition
   "name"
   (lambda (document)
     (getf (doc-value document) :name))))

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

(test global-index-definitions-rehydrate-on-open
  (let ((path #P"/tmp/test-tek9-global-index-definitions/"))
    (when (uiop:directory-exists-p path)
      (uiop:delete-directory-tree path :validate t))
    (let ((*index-definitions* (list (dtype-index-definition)))
          (db nil))
      (unwind-protect
           (progn
             (setf db (open-database (new-database "test" :path path)))
             (is (secondary-index-by-name db "dtype"))
             (put db (new-document :id "p1"
                                   :value '(:dtype "person" :name "Ada")))
             (close-database db)

             ;; A fresh DATABASE object automatically recreates the process-local
             ;; index definition while reusing the durable LMDB postings.
             (setf db (open-database (new-database "test" :path path)))
             (is (secondary-index-by-name db "dtype"))
             (is (equal '("p1")
                        (index-document-ids db "dtype" "person"))))
        (when db
          (close-database db))))))

(test database-index-definitions-override-global-registry
  (let ((*index-definitions* (list (dtype-index-definition)))
        (db nil))
    (unwind-protect
         (progn
           (setf db
                 (open-database
                  (new-database
                   "test"
                   :path #P"/tmp/test-tek9-index-definition-override/"
                   :index-definitions (list (name-index-definition)))))
           (is (null (secondary-index-by-name db "dtype")))
           (is (secondary-index-by-name db "name")))
      (when db
        (close-database db)))))

(test open-database-index-definitions-override-is-sticky
  (let ((*index-definitions* (list (dtype-index-definition)))
        (db (new-database
             "test"
             :path #P"/tmp/test-tek9-open-index-definition-override/"
             :index-definitions nil)))
    (unwind-protect
         (progn
           (open-database db :index-definitions (list (name-index-definition)))
           (is (secondary-index-by-name db "name"))
           (is (null (secondary-index-by-name db "dtype")))
           (close-database db)

           ;; OPEN-DATABASE stores its explicit override on the handle, so the
           ;; same configured DB object can be reopened without repeating it.
           (open-database db)
           (is (secondary-index-by-name db "name"))
           (is (null (secondary-index-by-name db "dtype"))))
      (close-database db))))
