(in-package :tek9)

(defvar +main-name+ "std" "Main database name for a LMDB env.")

(defvar *db* nil
  "Databse var.")


;; NOTE Design documents are what "defines" a view
;; In my database a design document can have multiple views which infact are their own lmdb datbases
;; The layout of the database dfir is such
;; /tek9-database/
;; .../database-1/
;; ....../view-1/
;; Databse deisngs are stored in the main database but the key is prefixed with design/<name>


;; A view itself is just a map reduce that has the results of it written to a lmdb database
;; it consists of a map-fn and a reduce-fn


(defclass database ()
  ((path :initarg :path :initform nil :accessor db-path)
   (env :initarg :env :initform nil :accessor db-env)
   (name :initarg :name :initform "" :accessor db-name)
   (open :initform nil :accessor db-open?)
   (views :initarg :views :accessor db-views :initform (dict))
   (document-count :initarg :count :initform 0 :accessor db-count)
   (changes :initform (make-array 500 :element-type 'string  :adjustable t) :accessor db-changed)
   (size :initform (* 1024 1024) :initarg :size :accessor db-max-size :type integer)))


(defmethod db-is-open-p ((db database))
  (db-open? db))

;;; ENCODING SECTION
;; NOTE This $%* is a helper function that just decodes stuff, its a thing from nim.

(defun %* (entry)
  (cpk:encode entry))
;; NOTE This $ is a helper function that just decodes stuff, its a thing from nim.

(defun $ (entry)
  (cpk:decode entry))

;;; DATABASE CRRATION/DELETION
;; Create a new database instance

(defun new-database (name &key (path (uiop:parse-unix-namestring "./tek9-database/")) (max-size (* 1024 1024)))
  (uiop:ensure-all-directories-exist (list path))
  (make-instance 'database :name name :path path :size max-size))


(defmethod open-database ((db database) &key (max-dbs 10))
  (let* ((env (db-env db)))
    (setf (db-open? db) t)
    (setf (db-env db) (lmdb:open-env (db-path db)  :if-does-not-exist :create :max-dbs max-dbs :map-size (db-max-size db)))
    db))




(defmethod close-database ((db database))
  (let ((env (db-env db)))
    (when (db-is-open-p db))
    (setf (db-open? db) nil)
    (lmdb:close-env env)
    (setf (db-env db) nil)))


(defmacro with-database ((database &key (write nil) (sync t) (meta-sync t)) &body body)
  `(let* ((env (db-env ,database)))
     (lmdb:with-txn (:env env :write ,write :sync ,sync :meta-sync ,meta-sync)
       ,@body
       (lmdb:commit-txn env))))




(defun map-database (database &key (write nil) (sync t) (meta-sync t) (map-fn 'list) (database-name +main-name+))
  (let ((db (lmdb:get-db database-name :env (db-env database))))
    (with-database (database :write write :meta-sync meta-sync :sync sync)
      (lmdb:do-db (key value db)
        (funcall map-fn key ($ value))))))
