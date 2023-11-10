(in-package :tek9)

(defvar +throw-view-errors+ t "Should we catch view errors or throw a tantrum? set to T to error!")

(define-condition view-error (error)
  ((view-map :initarg :map :reader view-map)
   (view-reduce :initarg :reduce :reader view-reduce))
  (:documentation "Error thrown when there is a bug in a view."))


(defclass database-view ()
  ((name :initform "" :type string :accessor view-name :initarg :name)
   (map-fn :initform nil :accessor view-map :initarg :map)
   (reduce-fn :initform nil :accessor view-reduce :initarg :reduce)))


(defmethod add-view ((db database) (view database-view))
  (setf (db-views db) (dict* (db-views db) (view-name view) view))
  (save-database-meta db))


;;; ENCODING VIEWS
(defmethod conspack:encode-object append
    ((object database-view) &key &allow-other-keys)
  (conspack:slots-to-alist (object)
    name map-fn reduce-fn))

(defmethod conspack:decode-object-initialize progn
    ((object database-view) class alist &key &allow-other-keys)
  (declare (ignore class))
  (alist-to-slots (alist object)
    name map-fn reduce-fn))


(defun new-view (name map-fn reduce-fn)
  (make-instance 'database-view :map map-fn :reduce reduce-fn :name name))

(defmacro define-map (view &body body)
  `(setf (view-map ,view)
         (lambda (doc)
           (let ((result nil))
             (defun emit (key val)
               (push (cons key val) result))
             ,@body
             result))))

(defmacro define-reduce (view &body body)
  `(setf (view-reduce ,view)
         (lambda (doc)
           (let ((result nil))
             ,@body
             result))))


;; Create the VIEW's database. DATABASE is a DATABASE object. Returns a DATABASE OBJECT
(defun create-view-db (database view)
  (lmdb:get-db (view-name view) :env (db-env database)))

;; NOTE should I delete the view on update? im thinking so yes.
(defun delete-view (database view)
  (lmdb:drop-db (view-name) (db-path database) :delete t))



(declaim (inline insert-results))
(defun insert-results (database view result)
  (let ((db (create-view-db database view))))
  (loop for (key val) in results
        do (lmdb:put key val)))


;;; Applys the map-fn of the view against the database
;; (declaim (inline apply-view))           ;
;; (defun apply-view (database view)
;;   (let* ((env (db-env database))
;;          (main-db (lmdb:get-db +main-name+)))
;;     (lmdb:with-txn (:env env)
;;       (lmdb:do-db (key val main-db)
;;         (handler-case
;;             (let ((result (funcall (view-map view) ($ val))))
;;               (insert-results database view results))
;;           (error (x)
;;             (when +throw-view-errors+
;;               (error view-error))))))))

(defun apply-view (database view doc-ids)
  (let* ((env (db-env database))
         (main-db (lmdb:get-db +main-name+))
         (docs (lmdb:with-txn (:env env :write nil)
                 (loop for id in doc-ids
                       collect ($ (lmdb:g3t main-db id)))
                 (lmdb:commit-txn env))))

    (lmdb:with-txn (:env env))
    (let ((results (mapcar (lambda (document)
                             (apply #'view-map document))
                           docs)))
      (mapcar  #'(lambda (result)
                   (insert-results main-db view result))
               results))))



(defmacro with-views (database views &body body)
  `(let* ((env (db-env ,database)))
     (lmdb:with-txn (:env db-env)
       ,@body
       (loop for view in ,views
             do (apply-view ,database view (db-changed ,database)))
       (lmdb:commit-txn env))))
