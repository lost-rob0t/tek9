(in-package :tek9)



(defclass node ()
  (
   (id :initarg :id :initform (make-key-id) :accessor node-id)
   (props :initarg :props :initform nil :accessor node-props)
   (edges :initarg :node-edges :initform nil :accessor node-edges)))

(defclass edge ()
  ((source :initarg :source :initform nil :accessor edge-source)
   (predicate :initarg :predicate :initform :child :accessor edge-predicate)
   (target :initarg :target :initform nil :accessor edge-target)))

(conspack:defencoding node
  id edges)

(conspack:defencoding edge
  source predicate target)

(defun get-default-graph-db ()
  (format nil "graph-~a" +main-name+))

(defun get-graph-db (database &key (database-name (get-default-graph-db)))
  (let ((env (db-env database)))
    (lmdb:get-db database-name :env env)))


(defun fetch-node (database id &key (database-name (get-default-graph-db)))
  (let* ((env (db-env database)))
    (lmdb:with-txn (:env env :write nil)
      (fetch* database id :database-name database-name))))


(defun fetch-bulk-nodes (database ids &key (database-name (get-default-graph-db)))
  (let* ((env (db-env database))
         (db (lmdb:get-db database-name :env env)))
    (lmdb:with-txn (:env env :write nil)
      (lmdb:with-cursor (cur db)
        (loop for id in ids
              do (lmdb:cursor-set-key id cur)
              collect ($ (lmdb:cursor-value cur)))))))


(defun add-node-edge (node edge)
  (setf (node-edges node) (push edge (node-edges node))))


;; Adds a edge to a node
(defun put-edge (cursor edge)
  (lmdb:cursor-set-key (edge-target edge))
  (let ((node (or  (lmdb:cursor-value cursor) (make-instance 'node :id node-id))))
    (add-node-edge node edge)
    (lmdb:cursor-put (edge-target edge) node cursor)))


(defun put-edges (database edges &key (database-name (get-default-graph-db)))
  (let* ((env (db-env database))
         (db (lmdb:get-db database-name :env env))
         (cur (lmdb:cursor-db db)))
    (lmdb:with-txn (:env env)
      (loop for edge in edges
            do (put-edge cur))
      (lmdb:commit-txn))))


(defun put-edges* (database edge-list &key  (database-name (get-default-graph-db)))
  (let* ((verts (loop for (source target predicate) in edge-list
                      collect (make-instance 'edge :source source :target target :predicate predicate))))
    (put-edges database verts :database-name database-name)))




(defun fetch-node-neighbors (database node-id &key (database-name (get-default-graph-db)))
  (let* ((env (db-env database))
         (db (lmdb:get-db database-name))
         (cur (lmdb:cursor-db db))
         (edges (node-edges (progn (lmdb:cursor-set-key node-id cur) ($ (lmdb:cursor-value cur))))))
    (lmdb:with-txn (:env env :write nil)
      (loop for target in edges
            for id = (edge-target target)
            do (lmdb:cursor-set-key id cur)
            collect ($ (lmdb:cursor-value cur))))))
