(in-package :tek9)

(defclass node ()
  ((id :initarg :id :initform (make-key-id) :accessor node-id)
   (props :initarg :props :initform nil :accessor node-props)
   (edges :initarg :node-edges :initform nil :accessor node-edges)))

(defclass edge ()
  ((source :initarg :source :initform nil :accessor edge-source)
   (predicate :initarg :predicate :initform :child :accessor edge-predicate)
   (target :initarg :target :initform nil :accessor edge-target)))

(conspack:defencoding node
  id props edges)

(conspack:defencoding edge
  source predicate target)

(defun get-default-graph-db ()
  (format nil "graph-~a" +main-name+))

(defun %graph-edge-db-name (database-name)
  (format nil "~a/edges" database-name))

(defun %graph-out-db-name (database-name)
  (format nil "~a/out" database-name))

(defun %graph-in-db-name (database-name)
  (format nil "~a/in" database-name))

(defun get-graph-db (database &key (database-name (get-default-graph-db)))
  (database-db database database-name
               :key-encoding :utf-8
               :value-encoding :octets))

(defun %graph-edge-db (database database-name)
  (database-db database (%graph-edge-db-name database-name)
               :key-encoding :utf-8
               :value-encoding :octets))

(defun %graph-out-db (database database-name)
  (database-db database (%graph-out-db-name database-name)
               :key-encoding :utf-8
               :value-encoding :utf-8
               :dupsort t))

(defun %graph-in-db (database database-name)
  (database-db database (%graph-in-db-name database-name)
               :key-encoding :utf-8
               :value-encoding :utf-8
               :dupsort t))

(defun ensure-graph-dbs (database database-name)
  "Open and cache every LMDB DBI needed by one graph."
  (values (get-graph-db database :database-name database-name)
          (%graph-edge-db database database-name)
          (%graph-out-db database database-name)
          (%graph-in-db database database-name)))

(defun edge-key (edge)
  "Return an unambiguous stable storage key for EDGE."
  (let ((source (princ-to-string (edge-source edge)))
        (predicate (princ-to-string (edge-predicate edge)))
        (target (princ-to-string (edge-target edge))))
    (format nil "~d:~a~d:~a~d:~a"
            (length source) source
            (length predicate) predicate
            (length target) target)))

(defun %decode-document-or-object (bytes)
  (and bytes ($ bytes)))

(defun put-node (database node &key (database-name (get-default-graph-db)))
  (let ((db (get-graph-db database :database-name database-name)))
    (with-database (database :write t)
      (lmdb:put db (node-id node) (%* node)))
    node))

(defun put-nodes (database nodes &key (database-name (get-default-graph-db)) sorted)
  (let ((db (get-graph-db database :database-name database-name)))
    (with-database (database :write t)
      (dolist (node nodes)
        (lmdb:put db (node-id node) (%* node) :append sorted)))
    nodes))

(defun fetch-node (database id &key (database-name (get-default-graph-db)))
  (let ((db (get-graph-db database :database-name database-name)))
    (with-database (database)
      (%decode-document-or-object (lmdb:g3t db id)))))

(defun fetch-bulk-nodes (database ids &key (database-name (get-default-graph-db)))
  (let ((db (get-graph-db database :database-name database-name)))
    (with-database (database)
      (lmdb:with-cursor (cursor db)
        (loop for id in ids
              collect
              (multiple-value-bind (bytes found)
                  (lmdb:cursor-set-key id cursor)
                (and found (%decode-document-or-object bytes))))))))

(defun add-node-edge (node edge)
  "Compatibility helper. Durable adjacency is stored in LMDB DUPSORT databases."
  (push edge (node-edges node))
  node)

(defun put-edge (database edge &key (database-name (get-default-graph-db)))
  (put-edges database (list edge) :database-name database-name)
  edge)

(defun put-edges (database edges &key (database-name (get-default-graph-db)))
  "Persist EDGES and both adjacency directions atomically.

The adjacency databases are DUPSORT keyspaces, so neighbor lookup is O(log N +
degree) rather than a full graph scan or node read-modify-write."
  (multiple-value-bind (nodes edge-db out-db in-db)
      (ensure-graph-dbs database database-name)
    (declare (ignore nodes))
    (with-database (database :write t)
      (dolist (edge edges)
        (let ((key (edge-key edge)))
          (lmdb:put edge-db key (%* edge))
          (lmdb:put out-db
                    (edge-source edge)
                    key
                    :dupdata nil
                    :key-exists-error-p nil)
          (lmdb:put in-db
                    (edge-target edge)
                    key
                    :dupdata nil
                    :key-exists-error-p nil)))))
  edges)

(defun put-edges* (database edge-list &key (database-name (get-default-graph-db)))
  (put-edges database
             (loop for (source target predicate) in edge-list
                   collect (make-instance 'edge
                                          :source source
                                          :target target
                                          :predicate predicate))
             :database-name database-name))

(defun fetch-node-edge-ids (database node-id
                            &key
                              (database-name (get-default-graph-db))
                              incoming)
  (multiple-value-bind (nodes edge-db out-db in-db)
      (ensure-graph-dbs database database-name)
    (declare (ignore nodes edge-db))
    (let ((adjacency (if incoming in-db out-db))
          (ids nil))
      (with-database (database)
        (lmdb:do-db-dup (edge-id adjacency node-id)
          (push edge-id ids)))
      (nreverse ids))))

(defun fetch-node-edges (database node-id
                         &key
                           (database-name (get-default-graph-db))
                           incoming)
  (multiple-value-bind (nodes edge-db out-db in-db)
      (ensure-graph-dbs database database-name)
    (declare (ignore nodes))
    (let ((adjacency (if incoming in-db out-db))
          (edges nil))
      (with-database (database)
        (lmdb:do-db-dup (edge-id adjacency node-id)
          (let ((bytes (lmdb:g3t edge-db edge-id)))
            (when bytes
              (push (%decode-document-or-object bytes) edges)))))
      (nreverse edges))))

(defun fetch-node-neighbors (database node-id
                             &key
                               (database-name (get-default-graph-db))
                               incoming)
  "Return neighboring NODE objects using one LMDB snapshot and adjacency indexes."
  (multiple-value-bind (nodes edge-db out-db in-db)
      (ensure-graph-dbs database database-name)
    (let ((adjacency (if incoming in-db out-db))
          (neighbors nil))
      (with-database (database)
        (lmdb:do-db-dup (edge-id adjacency node-id)
          (let* ((edge-bytes (lmdb:g3t edge-db edge-id))
                 (edge (and edge-bytes
                            (%decode-document-or-object edge-bytes))))
            (when edge
              (let* ((neighbor-id (if incoming
                                      (edge-source edge)
                                      (edge-target edge)))
                     (node-bytes (lmdb:g3t nodes neighbor-id)))
                (when node-bytes
                  (push (%decode-document-or-object node-bytes)
                        neighbors)))))))
      (nreverse neighbors))))

(defun delete-edge (database edge &key (database-name (get-default-graph-db)))
  "Delete EDGE and both adjacency references atomically."
  (multiple-value-bind (nodes edge-db out-db in-db)
      (ensure-graph-dbs database database-name)
    (declare (ignore nodes))
    (let ((key (edge-key edge)))
      (with-database (database :write t)
        (lmdb:del edge-db key)
        (lmdb:del out-db (edge-source edge) :value key)
        (lmdb:del in-db (edge-target edge) :value key))))
  t)
