(in-package :tek9)

(defparameter +graph-node-db+ "graph/nodes")
(defparameter +graph-edge-db+ "graph/edges")
(defparameter +graph-out-db+ "graph/out")
(defparameter +graph-in-db+ "graph/in")
(defparameter +graph-out-predicate-db+ "graph/out-predicate")
(defparameter +graph-in-predicate-db+ "graph/in-predicate")

(defclass node ()
  ((id :initarg :id :initform (make-key-id) :accessor node-id)
   (props :initarg :props :initform nil :accessor node-props)
   ;; Compatibility only. Durable adjacency lives in LMDB indexes.
   (edges :initarg :node-edges :initform nil :accessor node-edges)))

(defclass edge ()
  ((id :initarg :id :initform (make-key-id) :accessor edge-id)
   (source :initarg :source :initform nil :accessor edge-source)
   (predicate :initarg :predicate :initform :child :accessor edge-predicate)
   (target :initarg :target :initform nil :accessor edge-target)))

(conspack:defencoding node
  id props edges)

(conspack:defencoding edge
  id source predicate target)

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

(defun %graph-edge-key (graph-name edge-id)
  (%composite-key graph-name edge-id))

(defun %graph-adjacency-key (graph-name node-id)
  (%composite-key graph-name node-id))

(defun %graph-predicate-key (graph-name node-id predicate)
  (%composite-key graph-name node-id predicate))

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

(defun %graph-dupsort-db (database name)
  (database-db database name
               :key-encoding :utf-8
               :value-encoding :utf-8
               :dupsort t))

(defun ensure-graph-dbs (database)
  "Open and cache shared graph DBIs.

Adjacency stores compact edge IDs as DUPSORT values. Predicate-specific
keyspaces avoid scanning all incident edges when the relationship type is
known."
  (values (get-graph-db database)
          (%graph-edge-db database)
          (%graph-dupsort-db database +graph-out-db+)
          (%graph-dupsort-db database +graph-in-db+)
          (%graph-dupsort-db database +graph-out-predicate-db+)
          (%graph-dupsort-db database +graph-in-predicate-db+)))

(defun edge-key (edge)
  "Return EDGE's stable logical identity."
  (edge-id edge))

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

(defun %put-adjacency (db key edge-id)
  (lmdb:put db key edge-id :dupdata nil :key-exists-error-p nil))

(defun put-edges (database edges &key (database-name (get-default-graph-db)))
  "Persist EDGES and adjacency indexes atomically.

Each edge has its own compact ID, so parallel edges are preserved. General and
predicate-specific inbound/outbound indexes are updated in the same LMDB
transaction as the edge record."
  (multiple-value-bind (nodes edge-db out-db in-db out-predicate-db in-predicate-db)
      (ensure-graph-dbs database)
    (declare (ignore nodes))
    (with-database (database :write t)
      (dolist (edge edges)
        (let ((id (edge-id edge)))
          (lmdb:put edge-db (%graph-edge-key database-name id) (%* edge))
          (%put-adjacency out-db
                          (%graph-adjacency-key database-name (edge-source edge))
                          id)
          (%put-adjacency in-db
                          (%graph-adjacency-key database-name (edge-target edge))
                          id)
          (%put-adjacency out-predicate-db
                          (%graph-predicate-key database-name
                                                (edge-source edge)
                                                (edge-predicate edge))
                          id)
          (%put-adjacency in-predicate-db
                          (%graph-predicate-key database-name
                                                (edge-target edge)
                                                (edge-predicate edge))
                          id)))))
  edges)

(defun put-edges* (database edge-list &key (database-name (get-default-graph-db)))
  (put-edges database
             (loop for (source target predicate) in edge-list
                   collect (make-instance 'edge
                                          :source source
                                          :target target
                                          :predicate predicate))
             :database-name database-name))

(defun %adjacency-db-and-key (database database-name node-id incoming predicate)
  (multiple-value-bind (nodes edge-db out-db in-db out-predicate-db in-predicate-db)
      (ensure-graph-dbs database)
    (declare (ignore nodes edge-db))
    (if predicate
        (values (if incoming in-predicate-db out-predicate-db)
                (%graph-predicate-key database-name node-id predicate))
        (values (if incoming in-db out-db)
                (%graph-adjacency-key database-name node-id)))))

(defun fetch-node-edge-ids (database node-id
                            &key
                              (database-name (get-default-graph-db))
                              incoming
                              predicate)
  "Return compact edge IDs incident on NODE-ID.

When PREDICATE is supplied Tek9 uses the predicate-specific adjacency B+tree
instead of filtering the node's entire incident edge set."
  (multiple-value-bind (adjacency adjacency-key)
      (%adjacency-db-and-key database database-name node-id incoming predicate)
    (let ((ids nil))
      (with-database (database)
        (lmdb:do-db-dup (id adjacency adjacency-key)
          (push id ids)))
      (nreverse ids))))

(defun fetch-node-edges (database node-id
                         &key
                           (database-name (get-default-graph-db))
                           incoming
                           predicate)
  (multiple-value-bind (nodes edge-db out-db in-db out-predicate-db in-predicate-db)
      (ensure-graph-dbs database)
    (declare (ignore nodes out-db in-db out-predicate-db in-predicate-db))
    (multiple-value-bind (adjacency adjacency-key)
        (%adjacency-db-and-key database database-name node-id incoming predicate)
      (let ((edges nil))
        (with-database (database)
          (lmdb:do-db-dup (id adjacency adjacency-key)
            (let ((bytes (lmdb:g3t edge-db (%graph-edge-key database-name id))))
              (when bytes
                (push (%decode-document-or-object bytes) edges)))))
        (nreverse edges)))))

(defun fetch-node-neighbors (database node-id
                             &key
                               (database-name (get-default-graph-db))
                               incoming
                               predicate)
  "Return neighboring NODE objects in one LMDB snapshot.

PREDICATE selects the predicate-specific adjacency index when supplied."
  (multiple-value-bind (nodes edge-db out-db in-db out-predicate-db in-predicate-db)
      (ensure-graph-dbs database)
    (declare (ignore out-db in-db out-predicate-db in-predicate-db))
    (multiple-value-bind (adjacency adjacency-key)
        (%adjacency-db-and-key database database-name node-id incoming predicate)
      (let ((neighbors nil))
        (with-database (database)
          (lmdb:do-db-dup (id adjacency adjacency-key)
            (let* ((edge-bytes
                     (lmdb:g3t edge-db (%graph-edge-key database-name id)))
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
        (nreverse neighbors)))))

(defun delete-edge (database edge &key (database-name (get-default-graph-db)))
  "Delete EDGE and all adjacency references atomically."
  (multiple-value-bind (nodes edge-db out-db in-db out-predicate-db in-predicate-db)
      (ensure-graph-dbs database)
    (declare (ignore nodes))
    (let ((id (edge-id edge)))
      (with-database (database :write t)
        (lmdb:del edge-db (%graph-edge-key database-name id))
        (lmdb:del out-db
                  (%graph-adjacency-key database-name (edge-source edge))
                  :value id)
        (lmdb:del in-db
                  (%graph-adjacency-key database-name (edge-target edge))
                  :value id)
        (lmdb:del out-predicate-db
                  (%graph-predicate-key database-name
                                        (edge-source edge)
                                        (edge-predicate edge))
                  :value id)
        (lmdb:del in-predicate-db
                  (%graph-predicate-key database-name
                                        (edge-target edge)
                                        (edge-predicate edge))
                  :value id))))
  t)
