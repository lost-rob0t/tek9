(in-package :tek9-tests)
(in-suite :tek9-tests)

(test create-doc
  (is (string= (doc-id (new-document :value "test" :id "test-er")) "test-er"))
  (is (string= (doc-value (new-document :value "test")) "test")))

(test insert-doc
  (is (string= (progn
                 (let ((doc (new-document :id "test-insert" :value "test")))
                   (put *db* doc)
                   (fetch* *db* "test-insert"))) "test"))
  (is (string= "test-insert1" (progn
                                (put* *db* "test-insert1" :id "test02")
                                (fetch* *db* "test02")))))




(test insert-bulk
  (is (string= "bulk-insert" (progn
                               (let ((docs (loop for i from 0 to 1000 collect (new-document :id (format nil "~A" i) :value "bulk-insert"))))
                                 (put-bulk *db* docs)
                                 (fetch* *db* "1")))))
  (is (= 0 (progn
             (let ((docs (loop for i from 1001 to 2000
                               collect (list (format nil "~a" i) 0))))
               (put-bulk* *db* docs)
               (fetch* *db* "1500"))))))
(test fetch-bulk
  (is (string= "bulk-insert" (progn
                               (let* ((docs
                                        (loop for i from 0 to 2000 collect (format nil "~a"  i)))
                                      (documents (fetch-bulk* *db* docs)))
                                 (cdr (nth 500 documents)))))))
