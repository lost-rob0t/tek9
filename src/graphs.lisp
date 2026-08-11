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

(defun %decode-key-part (encoded start)
  "Decode one length-prefixed Tek9 key part beginning at START."
  (let ((colon (position #\: encoded :start start)))
    (unless colon
      (error "Malformed Tek9 composite key ~S." encoded))
    (let* ((length (parse-integer encoded :start start :end colon))
           (value-start (1+ colon))
           (value-end (+ value-start length)))
      (when (> value-end (length encoded))
        (error "Malformed Tek9 composite key ~S." encoded))
      (values (subseq encoded value-start value-end) value-end))))

(defun %adjacency-value (neighbor-id edge-id)
  "Encode the neighbor and edge identity into one DUPSORT value."
  (%composite-key neighbor-id edge-id))

(defun %decode-adjacency-value (encoded)
  "Return NEIGHBOR-ID and EDGE-ID from an adjacency DUPSORT value."
  (multiple-value-bind (neighbor-id next)
      (%decode-key-part encoded 0)
    (multiple-value-bind (edge-id end)
        (%decode-key-part encoded next)
      (unless (= end (length encoded))
        (error "Malformed Tek9 adjacency value ~S." encoded))
      (values neighbor-id edge-id))))

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

Adjacency stores (neighbor-id, edge-id) as the DUPSORT value. This lets graph
neighbor traversal jump directly from the adjacency index to the neighboring
node while edge-object traversal still resolves the edge id. Predicate-specific
keyspaces avoid scanning unrelated relationship types."
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
  "Resolve arbitrary node ids in one snapshot with direct MDB_GET operations."
  (let ((db (get-graph-db database)))
    (with-database (database)
      (loop for id in ids
            for bytes = (lmdb:g3t db (%graph-node-key database-name id))
            collect (%decode-document-or-object bytes)))))

(defun add-node-edge (node edge)
  "Compatibility helper. Durable adjacency is stored in LMDB DUPSORT databases."
  (push edge (node-edges node))
  node)

(defun put-edge (database edge &key (database-name (get-default-graph-db)))
  (put-edges database (list edge) :database-name database-name)
  edge)

(defun %put-adjacency (db key neighbor-id edge-id)
  (lmdb:put db
            key
            (%adjacency-value neighbor-id edge-id)
            :dupdata nil
            :key-exists-error-p nil))

(defun put-edges (database edges &key (database-name (get-default-graph-db)))
  "Persist EDGES and adjacency indexes atomically.

Each edge has its own compact ID, so parallel edges are preserved. General and
predicate-specific inbound/outbound indexes are updated in the same LMDB
transaction as the edge record. Adjacency values carry the neighbor id too, so
neighbor traversal does not need an intermediate edge-record lookup."
  (multiple-value-bind (nodes edge-db out-db in-db out-predicate-db in-predicate-db)
      (ensure-graph-dbs database)
    (declare (ignore nodes))
    (with-database (database :write t)
      (dolist (edge edges)
        (let ((id (edge-id edge))
              (source (edge-source edge))
              (target (edge-target edge))
              (predicate (edge-predicate edge)))
          (lmdb:put edge-db (%graph-edge-key database-name id) (%* edge))
          (%put-adjacency out-db
                          (%graph-adjacency-key database-name source)
                          target id)
          (%put-adjacency in-db
                          (%graph-adjacency-key database-name target)
                          source id)
          (%put-adjacency out-predicate-db
                          (%graph-predicate-key database-name source predicate)
                          target id)
          (%put-adjacency in-predicate-db
                          (%graph-predicate-key database-name target predicate)
                          source id)))))
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
  "Return only the adjacency DBI required for this lookup and its key."
  (if predicate
      (values (%graph-dupsort-db
               database
               (if incoming +graph-in-predicate-db+ +graph-out-predicate-db+))
              (%graph-predicate-key database-name node-id predicate))
      (values (%graph-dupsort-db
               database
               (if incoming +graph-in-db+ +graph-out-db+))
              (%graph-adjacency-key database-name node-id))))

(defun fetch-node-edge-ids (database node-id
                            &key
                              (database-name (get-default-graph-db))
                              incoming
                              predicate)
  "Return edge IDs incident on NODE-ID.

When PREDICATE is supplied Tek9 uses the predicate-specific adjacency B+tree
instead of filtering the node's entire incident edge set."
  (multiple-value-bind (adjacency adjacency-key)
      (%adjacency-db-and-key database database-name node-id incoming predicate)
    (let ((ids nil))
      (with-database (database)
        (lmdb:do-db-dup (value adjacency adjacency-key)
          (multiple-value-bind (neighbor-id edge-id)
              (%decode-adjacency-value value)
            (declare (ignore neighbor-id))
            (push edge-id ids))))
      (nreverse ids))))

(defun fetch-node-edges (database node-id
                         &key
                           (database-name (get-default-graph-db))
                           incoming
                           predicate)
  "Return edge objects incident on NODE-ID using indexed adjacency."
  (let ((edge-db (%graph-edge-db database)))
    (multiple-value-bind (adjacency adjacency-key)
        (%adjacency-db-and-key database database-name node-id incoming predicate)
      (let ((edges nil))
        (with-database (database)
          (lmdb:do-db-dup (value adjacency adjacency-key)
            (multiple-value-bind (neighbor-id edge-id)
                (%decode-adjacency-value value)
              (declare (ignore neighbor-id))
              (let ((bytes
                      (lmdb:g3t edge-db
                                (%graph-edge-key database-name edge-id))))
                (when bytes
                  (push (%decode-document-or-object bytes) edges))))))
        (nreverse edges)))))

(defun fetch-node-neighbors (database node-id
                             &key
                               (database-name (get-default-graph-db))
                               incoming
                               predicate)
  "Return neighboring NODE objects in one LMDB snapshot.

The adjacency value already contains the neighbor id, so this path performs one
adjacency scan plus one node lookup per result and does not read/decode edge
records. PREDICATE selects the predicate-specific adjacency index when supplied."
  (let ((nodes (get-graph-db database)))
    (multiple-value-bind (adjacency adjacency-key)
        (%adjacency-db-and-key database database-name node-id incoming predicate)
      (let ((neighbors nil))
        (with-database (database)
          (lmdb:do-db-dup (value adjacency adjacency-key)
            (multiple-value-bind (neighbor-id edge-id)
                (%decode-adjacency-value value)
              (declare (ignore edge-id))
              (let ((node-bytes
                      (lmdb:g3t nodes
                                (%graph-node-key database-name neighbor-id))))
                (when node-bytes
                  (push (%decode-document-or-object node-bytes)
                        neighbors))))))
        (nreverse neighbors)))))

(defun delete-edge (database edge &key (database-name (get-default-graph-db)))
  "Delete EDGE and all adjacency references atomically."
  (multiple-value-bind (nodes edge-db out-db in-db out-predicate-db in-predicate-db)
      (ensure-graph-dbs database)
    (declare (ignore nodes))
    (let* ((id (edge-id edge))
           (source (edge-source edge))
           (target (edge-target edge))
           (predicate (edge-predicate edge))
           (out-value (%adjacency-value target id))
           (in-value (%adjacency-value source id)))
      (with-database (database :write t)
        (lmdb:del edge-db (%graph-edge-key database-name id))
        (lmdb:del out-db
                  (%graph-adjacency-key database-name source)
                  :value out-value)
        (lmdb:del in-db
                  (%graph-adjacency-key database-name target)
                  :value in-value)
        (lmdb:del out-predicate-db
                  (%graph-predicate-key database-name source predicate)
                  :value out-value)
        (lmdb:del in-predicate-db
                  (%graph-predicate-key database-name target predicate)
                  :value in-value))))
  t)
