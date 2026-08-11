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
