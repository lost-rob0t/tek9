(in-package :tek9-tests)
(in-suite :tek9-tests)

(test opt-in-lmdb-map-auto-growth
  (let* ((path #P"/tmp/test-tek9-auto-grow/")
         (initial-size (* 2 1024 1024))
         (payload (make-string (* 4 1024 1024)
                               :initial-element #\x))
         (db nil))
    (when (uiop:directory-exists-p path)
      (uiop:delete-directory-tree path :validate t))
    (unwind-protect
         (progn
           (setf db
                 (open-database
                  (new-database "test"
                                :path path
                                :max-size initial-size
                                :auto-grow t)))
           (put* db (list :blob payload) :id "large")
           (is (> (db-max-size db) initial-size))
           (is (= (length payload)
                  (length (getf (fetch* db "large") :blob))))

           (close-database db)
           (setf db
                 (open-database
                  (new-database "test"
                                :path path
                                :max-size initial-size
                                :auto-grow t)))
           (is (= (length payload)
                  (length (getf (fetch* db "large") :blob)))))
      (when db
        (close-database db)))))
