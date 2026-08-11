(in-package :tek9)

(defconstant +graph-node-db+ "graph/nodes")
(defconstant +graph-edge-db+ "graph/edges")
(defconstant +graph-out-db+ "graph/out")
(defconstant +graph-in-db+ "graph/in")

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
  "Return the logical default graph name."
  "default")

(defun %key-part (value)
  (let ((string (princ-to-string value)))
    (format nil "~d:~a" (length string) string)))

(defun %composite-key (&rest values)
  "Encode VALUES into an unambiguous lexicographically stable string key."
  (with-output-to-string (stream)
    (dolist (value values)
      (write-string (%key-part value) stream))))

(defun %graph-node-key (graph-name node-id)
  (%composite-key graph-name node-id))

(defun %graph-adjacency-key (graph-name node-id)
  (%composite-key graph-name node-id))

(defun get-graph-db (database &key (database-name (get-default-graph-db)))
  "Return the shared graph-node DBI.

DATABASE-NAME is accepted for API compatibility; graph identity lives in the
composite key so Tek9 needs a constant number of LMDB named databases."
  (declare (ignore database-name))
  (database-db database +graph-node-db+
               :key-encoding :utf-8
               :value-encoding :octets))

(defun %graph-edge-db (database)
  (database-db database +graph-edge-db+
               :key-encoding :utf-8
               :value-encoding :octets))

(defun %graph-out-db (database)
  (database-db database +graph-out-db+
               :key-encoding :utf-8
               :value-encoding :utf-8
               :dupsort t))

(defun %graph-in-db (database)
  (database-db database +graph-in-db+
               :key-encoding :utf-8
               :value-encoding :utf-8
               :dupsort t))

(defun ensure-graph-dbs (database)
  "Open and cache the four shared graph DBIs."
  (values (get-graph-db database)
          (%graph-edge-db database)
          (%graph-out-db database)
          (%graph-in-db database)))

(defun edge-key (edge)
  "Return an unambiguous stable logical key for EDGE."
  (%composite-key (edge-source edge)
                  (edge-predicate edge)
                  (edge-target edge)))

(defun %graph-edge-key (graph-name edge)
  (%composite-key graph-name (edge-key edge)))

(defun %decode-document-or-object (bytes)
  (and bytes ($ bytes)))

(defun put-node (database node &key (database-name (get-default-graph-db)))
  (let ((db (get-graph-db database)))
    (with-database (database :write t)
      (lmdb:put db
                (%graph-node-key database-name (node-id node))
                (%* node)))
    node))

(defun put-nodes (database nodes &key (database-name (get-default-graph-db)) sorted)
  "Persist NODES in one transaction.

SORTED is a safe performance hint. Tek9 enables LMDB's append path only when
the first graph-qualified key is beyond the entire shared node DB."
  (let ((db (get-graph-db database)))
    (with-database (database :write t)
      (let ((append-p
              (and sorted
                   nodes
                   (%append-safe-p
                    db
                    (%graph-node-key database-name (node-id (first nodes)))))))
        (dolist (node nodes)
          (lmdb:put db
                    (%graph-node-key database-name (node-id node))
                    (%* node)
                    :append append-p))))
    nodes))

(defun fetch-node (database id &key (database-name (get-default-graph-db)))
  (let ((db (get-graph-db database)))
    (with-database (database)
      (%decode-document-or-object
       (lmdb:g3t db (%graph-node-key database-name id))))))

(defun fetch-bulk-nodes (database ids &key (database-name (get-default-graph-db)))
  (let ((db (get-graph-db database)))
    (with-database (database)
      (lmdb:with-cursor (cursor db)
        (loop for id in ids
              for storage-key = (%graph-node-key database-name id)
              collect
              (multiple-value-bind (bytes found)
                  (lmdb:cursor-set-key storage-key cursor)
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

All logical graphs share four physical LMDB named databases. Composite keys
keep graph namespaces isolated without paying four DBIs per graph."
  (multiple-value-bind (nodes edge-db out-db in-db)
      (ensure-graph-dbs database)
    (declare (ignore nodes))
    (with-database (database :write t)
      (dolist (edge edges)
        (let ((storage-key (%graph-edge-key database-name edge)))
          (lmdb:put edge-db storage-key (%* edge))
          (lmdb:put out-db
                    (%graph-adjacency-key database-name (edge-source edge))
                    storage-key
                    :dupdata nil
                    :key-exists-error-p nil)
          (lmdb:put in-db
                    (%graph-adjacency-key database-name (edge-target edge))
                    storage-key
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
      (ensure-graph-dbs database)
    (declare (ignore nodes edge-db))
    (let ((adjacency (if incoming in-db out-db))
          (adjacency-key (%graph-adjacency-key database-name node-id))
          (ids nil))
      (with-database (database)
        (lmdb:do-db-dup (edge-id adjacency adjacency-key)
          (push edge-id ids)))
      (nreverse ids))))

(defun fetch-node-edges (database node-id
                         &key
                           (database-name (get-default-graph-db))
                           incoming)
  (multiple-value-bind (nodes edge-db out-db in-db)
      (ensure-graph-dbs database)
    (declare (ignore nodes))
    (let ((adjacency (if incoming in-db out-db))
          (adjacency-key (%graph-adjacency-key database-name node-id))
          (edges nil))
      (with-database (database)
        (lmdb:do-db-dup (edge-id adjacency adjacency-key)
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
      (ensure-graph-dbs database)
    (let ((adjacency (if incoming in-db out-db))
          (adjacency-key (%graph-adjacency-key database-name node-id))
          (neighbors nil))
      (with-database (database)
        (lmdb:do-db-dup (edge-id adjacency adjacency-key)
          (let* ((edge-bytes (lmdb:g3t edge-db edge-id))
                 (edge (and edge-bytes
                            (%decode-document-or-object edge-bytes))))
            (when edge
              (let* ((neighbor-id (if incoming
                                      (edge-source edge)
                                      (edge-target edge)))
                     (node-bytes
                       (lmdb:g3t nodes
                                 (%graph-node-key database-name neighbor-id))))
                (when node-bytes
                  (push (%decode-document-or-object node-bytes)
                        neighbors)))))))
      (nreverse neighbors))))

(defun delete-edge (database edge &key (database-name (get-default-graph-db)))
  "Delete EDGE and both adjacency references atomically."
  (multiple-value-bind (nodes edge-db out-db in-db)
      (ensure-graph-dbs database)
    (declare (ignore nodes))
    (let ((storage-key (%graph-edge-key database-name edge)))
      (with-database (database :write t)
        (lmdb:del edge-db storage-key)
        (lmdb:del out-db
                  (%graph-adjacency-key database-name (edge-source edge))
                  :value storage-key)
        (lmdb:del in-db
                  (%graph-adjacency-key database-name (edge-target edge))
                  :value storage-key))))
  t)
