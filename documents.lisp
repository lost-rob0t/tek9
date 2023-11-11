(in-package :tek9)




;; XXX This is a var that stores a function that sets the uuid
;; By default it is set my cl-ulid
;; It must return bytes!
(defun make-key-id ()
  (multiple-value-bind (ulid-str ulid-bytes) (ulid:ulid) ulid-str))



;; Object to hold our Key value pairs in
;; Maybe it should be a struct or could just be a cons pair?
;; This is the main object that represents the document!
(defclass document ()
  ((id :initform (make-key-id) :type string :initarg :id :accessor doc-id)
   (value :initform nil :initarg :value :accessor doc-value)
   (changed :initform nil :initarg :changed :accessor doc-changed)))



(defun new-document (&key (id (make-key-id) ) (value nil))
  (make-instance 'document :id id :value value))


(defmethod conspack:encode-object append
    ((object document) &key &allow-other-keys)
  (conspack:slots-to-alist (object)
    id value changed))


(defmethod conspack:decode-object-initialize progn
    ((object document) class alist &key &allow-other-keys)
  (declare (ignore class))
  (alist-to-slots (alist object)
    id value changed))


;; Touch a document. this is how I keep track of changed documents!
;; It returns the document
(declaim (inline touch-document))
(defun touch-document (database document)
  (setf (doc-changed document) t)
  (setf (db-changed database) (push (doc-id document) (db-changed database)))
  document)


;; Put a key
(declaim (inline put))
(defun put (database document)
  (let* ((env (db-env database))
         (db (lmdb:get-db +main-name+ :env env))
         ;; Updating the changed flag!
         (document (touch-document database document)))
    (lmdb:with-txn (:env env :write t)
      (lmdb:put db (doc-id document) (%* document))
      (lmdb:commit-txn env))))

;; Magic function to also create the document "container" that holds it
(defun put* (database document &key (database-name +main-name+) (id (make-key-id)))
  (let ((doc (new-document :id id :value document)))
    (put database doc)))


;; Get a key. I dont get why the wrapper had to use g3t... fetch or find would also fit.
(declaim (inline fetch))
(defun fetch (database id)
  (let* ((env (db-env database))
         (db (lmdb:get-db +main-name+ :env env)))
    (lmdb:with-txn (:env env)
      ($ (lmdb:g3t db id)))))

;; Return The value
(defun fetch* (db id)
  (doc-value (fetch db id)))

;; Document here is just a json string
(defun put-json (db document)
  (let* ((db-env (db-env db))
         (db (lmdb:get-db +main-name+ :env db-env))
         (json-data (jsown:parse document))
         (id (jsown:val-safe json-data "id")))
    (put db (new-document :id (if id id (make-key-id)) :value json-data))))
