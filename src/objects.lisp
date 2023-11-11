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
   (views :initarg :views :accessor db-views :initform (dict))
   (document-count :initarg :count :initform 0 :accessor db-count)
   (changes :initform (make-array 500 :element-type 'string  :adjustable t) :accessor db-changed)))

;;; ENCODING SECTION
;; NOTE This $%* is a helper function that just decodes stuff, its a thing from nim.
(defun %* (entry)
  (cpk:encode entry))
;; NOTE This $ is a helper function that just decodes stuff, its a thing from nim.

(defun $ (entry)
  (cpk:decode entry))

;;; DATABASE CRRATION/DELETION
;; Create a new database instance
(defun new-database (name &key (path (uiop:parse-unix-namestring "./tek9-database/")) (meta nil))
  (uiop:ensure-all-directories-exist (list path))
  (make-instance 'database :name name :path path))

(defmethod open-database ((db database) &key (max-dbs 10))
  (let* ((env (db-env db)))
    (setf (db-env db) (lmdb:open-env (db-path db)  :if-does-not-exist :create :max-dbs max-dbs))
    db))




(defmethod close-database ((db database))
  (let ((env (db-env db)))
    (lmdb:close-env env)
    (setf (db-env db) nil)))

;; TODO This is not correct
;; (defmacro with-database ((database &optional (write nil) (sync t) (meta-sync t)) &body body)
;;   `(let ((env (db-env ,database)))
;;      (lmdb:with-txn (:env env) ,write ,sync ,meta-sync
;;        ,@body
;;        (lmdb:commit-txn env))))
