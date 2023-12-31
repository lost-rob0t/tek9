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

(export 'document)

(conspack:defencoding document
  id value changed)



(defun new-document (&rest keys-vals)
  (apply #'make-instance 'document keys-vals))



;; Touch a document. this is how I keep track of changed documents!
;; It returns the document

(defun touch-document (database document)
  (setf (doc-changed document) t)
  (setf (db-changed database) (push (doc-id document) (db-changed database)))
  document)


(defun untouch-document (document)
  (setf (doc-changed document) nil))

;; Put a key


(defun put (database document)
  (let* ((env (db-env database))
         (db (lmdb:get-db +main-name+ :env env))
         ;; Updating the changed flag!
         (document (touch-document database document)))
    (lmdb:with-txn (:env env :write t)
      (lmdb:put db (doc-id document) (%* document))
      (lmdb:commit-txn env))))

;; Magic function to also create the document "container" that holds it

(defun put* (database document &key (id (make-key-id)))
  (let ((doc (new-document :id id :value document :changed t)))
    (put database doc)))


(defun put-bulk (database documents &key (database-name +main-name+))
  (let* ((env (db-env database))
         (db (lmdb:get-db database-name :env env)))
    (lmdb:with-txn (:env env :write t)
      (loop for document in documents
            do (lmdb:put db (doc-id document) (%* document)))
      (lmdb:commit-txn env))))


(defun put-bulk* (database documents &key (database-name +main-name+))
  (put-bulk database (loop for  (key val) in documents
                           collect (new-document :id key :value val))
            :database-name database-name))


;; Document here is just a json string

(defun put-json (db document)
  (let* ((db-env (db-env db))
         (db (lmdb:get-db +main-name+ :env db-env))
         (json-data (jsown:parse document))
         (id (jsown:val-safe json-data "id")))
    (put db (new-document :id (if id id (make-key-id)) :value json-data))))



;; Get a key. I dont get why the wrapper had to use g3t... fetch or find would also fit.


(defun fetch (database id &key (database-name +main-name+))
  (let* ((env (db-env database))
         (db (lmdb:get-db database-name :env env :value-encoding :octets)))
    (lmdb:with-txn (:env env)
      ($ (lmdb:g3t db id)))))

;; Return The value

(defun fetch* (database id &key (database-name +main-name+))
  (doc-value (fetch database id :database-name database-name)))


(defun fetch-bulk (database document-ids &key (database-name +main-name+))
  (let* ((env (db-env database))
         (db (lmdb:get-db database-name :env env)))
    (lmdb:with-txn (:env env :write nil)
      (loop for id in document-ids
            collect (cons id ($ (lmdb:g3t db id)))))))

(defun fetch-bulk* (database document-ids &key (database-name +main-name+))
  (let ((results (fetch-bulk database document-ids)))
    (loop for result in results collect
                                (cons (car result) (doc-value (cdr result))))))
