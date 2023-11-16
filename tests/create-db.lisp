(in-package :tek9-tests)

;; Create a db at /tmp/test-tek9/
(defvar +test-path+ #P"/tmp/test-tek9/")
(defun setup-db (path)
  (when (uiop:directory-exists-p path)
    (uiop:delete-directory-tree path :validate t))
  (let ((db (new-database "test" :path path)))
    (open-database db)))


(defvar *db* (setup-db +test-path+))

(in-suite :tek9-tests)

(test create-db
      (finishes (setup-db #P"/tmp/test-tek9-1/")))
