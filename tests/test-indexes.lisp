(in-package :tek9-tests)
(in-suite :tek9-tests)

(test secondary-index
  (let ((db (setup-db #P"/tmp/test-tek9-index/")))
    (unwind-protect
         (progn
           (register-index db
                           "dtype"
                           (lambda (document)
                             (getf (doc-value document) :dtype)))
           (put-bulk db
                     (list (new-document :id "p1"
                                         :value '(:dtype "person" :name "Ada"))
                           (new-document :id "p2"
                                         :value '(:dtype "person" :name "Grace"))
                           (new-document :id "o1"
                                         :value '(:dtype "organization" :name "ACME"))))
           (is (= 2 (length (index-document-ids db "dtype" "person"))))
           (is (= 2 (length (select-index db "dtype" "person"))))
           (put db (new-document :id "p2"
                                 :value '(:dtype "organization" :name "Grace")))
           (is (= 1 (length (index-document-ids db "dtype" "person"))))
           (is (= 2 (length (index-document-ids db "dtype" "organization")))))
      (close-database db))))

(test secondary-index-range-duplicates
  (let ((db (setup-db #P"/tmp/test-tek9-index-range-duplicates/")))
    (unwind-protect
         (progn
           (register-index db
                           "tag"
                           (lambda (document)
                             (getf (doc-value document) :tag)))
           (put-bulk db
                     (list (new-document :id "a1" :value '(:tag "a" :n 1))
                           (new-document :id "a2" :value '(:tag "a" :n 2))
                           (new-document :id "b1" :value '(:tag "b" :n 3))))
           ;; A non-unique index is DUPSORT. The bounded range primitive must
           ;; enumerate duplicate postings rather than silently skipping them.
           (is (equal '((:tag "a" :n 1) (:tag "a" :n 2))
                      (select-index-range db "tag" "a" :end "a")))
           (is (= 1 (length
                     (select-index-range db "tag" "a" :end "a" :limit 1))))
           (is (= 2 (length
                     (select-index-range db "tag" "a" :end "a" :limit 2))))
           (is (equal '((:tag "a" :n 1)
                        (:tag "a" :n 2)
                        (:tag "b" :n 3))
                      (select-index-range db "tag" "a" :end "b"))))
      (close-database db))))

(test fast-index-rebuild
  (let ((db (setup-db #P"/tmp/test-tek9-fast-index/")))
    (unwind-protect
         (progn
           (put-bulk db
                     (list (new-document :id "d3" :value '(:tag "b"))
                           (new-document :id "d1" :value '(:tag "a"))
                           (new-document :id "d2" :value '(:tag "a"))))
           (register-index db
                           "tag"
                           (lambda (document)
                             (getf (doc-value document) :tag)))
           (rebuild-index-fast db "tag")
           (is (equal '("d1" "d2")
                      (index-document-ids db "tag" "a")))
           (is (equal '("d3")
                      (index-document-ids db "tag" "b"))))
      (close-database db))))

(test atomic-bulk-load
  (let ((db (setup-db #P"/tmp/test-tek9-bulk-load/")))
    (unwind-protect
         (progn
           (register-index db
                           "dtype"
                           (lambda (document)
                             (getf (doc-value document) :dtype)))
           (bulk-load db
                      (list (new-document :id "p2"
                                          :value '(:dtype "person" :name "Grace"))
                            (new-document :id "o1"
                                          :value '(:dtype "organization" :name "ACME"))
                            (new-document :id "p1"
                                          :value '(:dtype "person" :name "Ada"))))
           (is (equal '("p1" "p2")
                      (index-document-ids db "dtype" "person")))
           (is (equal "Ada" (getf (fetch* db "p1") :name)))
           (signals error
             (bulk-load db
                        (list (new-document :id "again"
                                            :value '(:dtype "person"))))))
      (close-database db))))

(test bulk-load-unique-rollback
  (let ((db (setup-db #P"/tmp/test-tek9-bulk-load-rollback/")))
    (unwind-protect
         (progn
           (register-index db
                           "email"
                           (lambda (document)
                             (getf (doc-value document) :email))
                           :unique t)
           (signals error
             (bulk-load db
                        (list (new-document :id "p1"
                                            :value '(:email "same@example.test"))
                              (new-document :id "p2"
                                            :value '(:email "same@example.test")))))
           (is (null (fetch db "p1")))
           (is (null (fetch db "p2")))
           (is (null (index-document-ids db "email" "same@example.test"))))
      (close-database db))))

(test primary-range
  (let ((db (setup-db #P"/tmp/test-tek9-range/")))
    (unwind-protect
         (progn
           (put-bulk db
                     (list (new-document :id "a" :value 1)
                           (new-document :id "b" :value 2)
                           (new-document :id "c" :value 3))
                     :sorted t)
           (is (equal '(("b" . 2) ("c" . 3))
                      (select-primary-range db "b" :end "c"))))
      (close-database db))))