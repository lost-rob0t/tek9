(in-package :tek9)

(defparameter +graph-meta-db+ "graph/v2/meta")
(defparameter +graph-node-map-db+ "graph/v2/node-map")
(defparameter +graph-edge-map-db+ "graph/v2/edge-map")
(defparameter +graph-predicate-map-db+ "graph/v2/predicate-map")
(defparameter +graph-node-db+ "graph/v2/nodes")
(defparameter +graph-edge-db+ "graph/v2/edges")
(defparameter +graph-out-db+ "graph/v2/out")
(defparameter +graph-in-db+ "graph/v2/in")
(defparameter +graph-out-predicate-db+ "graph/v2/out-predicate")
(defparameter +graph-in-predicate-db+ "graph/v2/in-predicate")

(defparameter +next-node-id-key+ "next-node-id")
(defparameter +next-edge-id-key+ "next-edge-id")
(defparameter +next-predicate-id-key+ "next-predicate-id")

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

(defstruct graph-dbis
  meta
  node-map
  edge-map
  predicate-map
  nodes
  edges
  out
  in
  out-predicate
  in-predicate)

(defun get-default-graph-db ()
  "Return the logical default graph name."
  "default")

(defun %key-part (value)
  (let ((string (princ-to-string value)))
    (format nil "~d:~a" (length string) string)))

(defun %composite-key (&rest values)
  "Encode external identifiers into an unambiguous string lookup key."
  (with-output-to-string (stream)
    (dolist (value values)
      (write-string (%key-part value) stream))))

(defun %external-node-key (graph-name node-id)
  (%composite-key graph-name node-id))

(defun %external-edge-key (graph-name edge-id)
  (%composite-key graph-name edge-id))

(defun %uint64-octets (value vector offset)
  "Write VALUE as big-endian uint64 into VECTOR at OFFSET."
  (check-type value (integer 0 #.(1- (expt 2 64))))
  (dotimes (index 8 vector)
    (setf (aref vector (+ offset index))
          (ldb (byte 8 (* 8 (- 7 index))) value))))

(defun %octets-uint64 (vector offset)
  "Read a big-endian uint64 from VECTOR at OFFSET."
  (let ((value 0))
    (dotimes (index 8 value)
      (setf value
            (logior (ash value 8)
                    (aref vector (+ offset index)))))))

(defun %uint64-pair (left right)
  "Return a fixed 16-byte big-endian pair."
  (let ((vector (make-array 16 :element-type '(unsigned-byte 8))))
    (%uint64-octets left vector 0)
    (%uint64-octets right vector 8)
    vector))

(defun %decode-uint64-pair (vector)
  (values (%octets-uint64 vector 0)
          (%octets-uint64 vector 8)))

(defun %predicate-adjacency-key (node-row predicate-row)
  (%uint64-pair node-row predicate-row))

(defun %adjacency-value (neighbor-row edge-row)
  (%uint64-pair neighbor-row edge-row))

(defun %graph-meta-db (database)
  (database-db database +graph-meta-db+
               :key-encoding :utf-8
               :value-encoding :uint64))

(defun %graph-node-map-db (database)
  (database-db database +graph-node-map-db+
               :key-encoding :utf-8
               :value-encoding :uint64))

(defun %graph-edge-map-db (database)
  (database-db database +graph-edge-map-db+
               :key-encoding :utf-8
               :value-encoding :uint64))

(defun %graph-predicate-map-db (database)
  (database-db database +graph-predicate-map-db+
               :key-encoding :utf-8
               :value-encoding :uint64))

(defun get-graph-db (database &key (database-name (get-default-graph-db)))
  "Return the shared internal node DBI.

DATABASE-NAME is accepted for API compatibility. Logical graph isolation is in
the external-id mapping; internal node row IDs are globally unique within the
Tek9 environment."
  (declare (ignore database-name))
  (database-db database +graph-node-db+
               :key-encoding :uint64
               :value-encoding :octets
               :integer-key t))

(defun %graph-edge-db (database)
  (database-db database +graph-edge-db+
               :key-encoding :uint64
               :value-encoding :octets
               :integer-key t))

(defun %graph-adjacency-db (database name)
  (database-db database name
               :key-encoding :uint64
               :value-encoding :octets
               :integer-key t
               :dupsort t
               :dupfixed t))

(defun %graph-predicate-adjacency-db (database name)
  (database-db database name
               :key-encoding :octets
               :value-encoding :octets
               :dupsort t
               :dupfixed t))

(defun ensure-graph-dbs (database)
  "Open and cache every physical graph DBI before entering a transaction."
  (make-graph-dbis
   :meta (%graph-meta-db database)
   :node-map (%graph-node-map-db database)
   :edge-map (%graph-edge-map-db database)
   :predicate-map (%graph-predicate-map-db database)
   :nodes (get-graph-db database)
   :edges (%graph-edge-db database)
   :out (%graph-adjacency-db database +graph-out-db+)
   :in (%graph-adjacency-db database +graph-in-db+)
   :out-predicate (%graph-predicate-adjacency-db
                   database +graph-out-predicate-db+)
   :in-predicate (%graph-predicate-adjacency-db
                  database +graph-in-predicate-db+)))

(defun %next-row-id (meta key)
  "Allocate one monotonically increasing uint64 row id in the active write txn."
  (let* ((current (or (lmdb:g3t meta key) 0))
         (next (1+ current)))
    (lmdb:put meta key next)
    next))

(defun %lookup-node-row (dbis graph-name external-id)
  (lmdb:g3t (graph-dbis-node-map dbis)
            (%external-node-key graph-name external-id)))

(defun %ensure-node-row (dbis graph-name external-id)
  (or (%lookup-node-row dbis graph-name external-id)
      (let ((row (%next-row-id (graph-dbis-meta dbis) +next-node-id-key+)))
        (lmdb:put (graph-dbis-node-map dbis)
                  (%external-node-key graph-name external-id)
                  row)
        row)))

(defun %lookup-edge-row (dbis graph-name external-id)
  (lmdb:g3t (graph-dbis-edge-map dbis)
            (%external-edge-key graph-name external-id)))

(defun %ensure-edge-row (dbis graph-name external-id)
  (or (%lookup-edge-row dbis graph-name external-id)
      (let ((row (%next-row-id (graph-dbis-meta dbis) +next-edge-id-key+)))
        (lmdb:put (graph-dbis-edge-map dbis)
                  (%external-edge-key graph-name external-id)
                  row)
        row)))

(defun %lookup-predicate-row (dbis predicate)
  (lmdb:g3t (graph-dbis-predicate-map dbis) (princ-to-string predicate)))

(defun %ensure-predicate-row (dbis predicate)
  (let ((key (princ-to-string predicate)))
    (or (lmdb:g3t (graph-dbis-predicate-map dbis) key)
        (let ((row (%next-row-id
                    (graph-dbis-meta dbis)
                    +next-predicate-id-key+)))
          (lmdb:put (graph-dbis-predicate-map dbis) key row)
          row))))

(defun edge-key (edge)
  "Return EDGE's stable external identity."
  (edge-id edge))

(defun %decode-document-or-object (bytes)
  (and bytes ($ bytes)))

(defun put-node (database node &key (database-name (get-default-graph-db)))
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database :write t)
      (let ((row (%ensure-node-row dbis database-name (node-id node))))
        (lmdb:put (graph-dbis-nodes dbis) row (%* node))))
    node))

(defun put-nodes (database nodes &key (database-name (get-default-graph-db)) sorted)
  "Persist NODES in one transaction using compact internal uint64 row IDs.

SORTED is retained for API compatibility; internal row IDs are allocated
monotonically, so new rows naturally follow LMDB's integer-key order."
  (declare (ignore sorted))
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database :write t)
      (dolist (node nodes)
        (let ((row (%ensure-node-row dbis database-name (node-id node))))
          (lmdb:put (graph-dbis-nodes dbis) row (%* node)))))
    nodes))

(defun fetch-node (database id &key (database-name (get-default-graph-db)))
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database)
      (let ((row (%lookup-node-row dbis database-name id)))
        (and row
             (%decode-document-or-object
              (lmdb:g3t (graph-dbis-nodes dbis) row)))))))

(defun fetch-bulk-nodes (database ids &key (database-name (get-default-graph-db)))
  "Resolve arbitrary external node IDs inside one LMDB snapshot."
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database)
      (loop for id in ids
            for row = (%lookup-node-row dbis database-name id)
            collect
            (and row
                 (%decode-document-or-object
                  (lmdb:g3t (graph-dbis-nodes dbis) row)))))))

(defun add-node-edge (node edge)
  "Compatibility helper. Durable adjacency is stored in LMDB DUPSORT databases."
  (push edge (node-edges node))
  node)

(defun put-edge (database edge &key (database-name (get-default-graph-db)))
  (put-edges database (list edge) :database-name database-name)
  edge)

(defun %put-adjacency (db key neighbor-row edge-row)
  (lmdb:put db
            key
            (%adjacency-value neighbor-row edge-row)
            :dupdata nil
            :key-exists-error-p nil))

(defun %delete-adjacency (db key neighbor-row edge-row)
  (lmdb:del db key :value (%adjacency-value neighbor-row edge-row)))

(defun %remove-edge-adjacency (dbis graph-name edge-row edge)
  "Remove EDGE's current adjacency entries in the active write transaction."
  (let* ((source-row (%lookup-node-row dbis graph-name (edge-source edge)))
         (target-row (%lookup-node-row dbis graph-name (edge-target edge)))
         (predicate-row (%lookup-predicate-row dbis (edge-predicate edge))))
    (when (and source-row target-row)
      (%delete-adjacency (graph-dbis-out dbis)
                         source-row target-row edge-row)
      (%delete-adjacency (graph-dbis-in dbis)
                         target-row source-row edge-row)
      (when predicate-row
        (%delete-adjacency (graph-dbis-out-predicate dbis)
                           (%predicate-adjacency-key source-row predicate-row)
                           target-row edge-row)
        (%delete-adjacency (graph-dbis-in-predicate dbis)
                           (%predicate-adjacency-key target-row predicate-row)
                           source-row edge-row)))))

(defun %insert-edge (dbis graph-name edge)
  "Insert or replace one EDGE in the active write transaction."
  (let* ((source-row (%lookup-node-row dbis graph-name (edge-source edge)))
         (target-row (%lookup-node-row dbis graph-name (edge-target edge))))
    (unless source-row
      (error "Unknown source node ~S in graph ~S."
             (edge-source edge) graph-name))
    (unless target-row
      (error "Unknown target node ~S in graph ~S."
             (edge-target edge) graph-name))
    (let* ((existing-row (%lookup-edge-row dbis graph-name (edge-id edge)))
           (edge-row (or existing-row
                         (%ensure-edge-row dbis graph-name (edge-id edge))))
           (predicate-row (%ensure-predicate-row dbis (edge-predicate edge))))
      (when existing-row
        (let ((previous-bytes (lmdb:g3t (graph-dbis-edges dbis) edge-row)))
          (when previous-bytes
            (%remove-edge-adjacency
             dbis graph-name edge-row
             (%decode-document-or-object previous-bytes)))))
      (lmdb:put (graph-dbis-edges dbis) edge-row (%* edge))
      (%put-adjacency (graph-dbis-out dbis)
                      source-row target-row edge-row)
      (%put-adjacency (graph-dbis-in dbis)
                      target-row source-row edge-row)
      (%put-adjacency (graph-dbis-out-predicate dbis)
                      (%predicate-adjacency-key source-row predicate-row)
                      target-row edge-row)
      (%put-adjacency (graph-dbis-in-predicate dbis)
                      (%predicate-adjacency-key target-row predicate-row)
                      source-row edge-row)
      edge-row)))

(defun put-edges (database edges &key (database-name (get-default-graph-db)))
  "Persist EDGES and every adjacency index atomically.

External strings are resolved once at the boundary. The hot graph keyspaces use
uint64 node/edge/predicate IDs and fixed 16-byte adjacency records."
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database :write t)
      (dolist (edge edges)
        (%insert-edge dbis database-name edge)))
    edges))

(defun put-edges* (database edge-list &key (database-name (get-default-graph-db)))
  (put-edges database
             (loop for (source target predicate) in edge-list
                   collect (make-instance 'edge
                                          :source source
                                          :target target
                                          :predicate predicate))
             :database-name database-name))

(defun %adjacency-db-and-key (dbis node-row predicate incoming)
  "Return the physical adjacency DBI and key for NODE-ROW."
  (if predicate
      (let ((predicate-row (%lookup-predicate-row dbis predicate)))
        (if predicate-row
            (values (if incoming
                        (graph-dbis-in-predicate dbis)
                        (graph-dbis-out-predicate dbis))
                    (%predicate-adjacency-key node-row predicate-row))
            (values nil nil)))
      (values (if incoming (graph-dbis-in dbis) (graph-dbis-out dbis))
              node-row)))

(defun fetch-node-edge-ids (database node-id
                            &key
                              (database-name (get-default-graph-db))
                              incoming
                              predicate)
  "Return external edge IDs incident on NODE-ID."
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database)
      (let ((node-row (%lookup-node-row dbis database-name node-id)))
        (unless node-row
          (return-from fetch-node-edge-ids nil))
        (multiple-value-bind (adjacency key)
            (%adjacency-db-and-key dbis node-row predicate incoming)
          (unless adjacency
            (return-from fetch-node-edge-ids nil))
          (let ((ids nil))
            (lmdb:do-db-dup (value adjacency key)
              (multiple-value-bind (neighbor-row edge-row)
                  (%decode-uint64-pair value)
                (declare (ignore neighbor-row))
                (let ((bytes (lmdb:g3t (graph-dbis-edges dbis) edge-row)))
                  (when bytes
                    (push (edge-id (%decode-document-or-object bytes)) ids)))))
            (nreverse ids)))))))

(defun fetch-node-edges (database node-id
                         &key
                           (database-name (get-default-graph-db))
                           incoming
                           predicate)
  "Return edge objects incident on NODE-ID."
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database)
      (let ((node-row (%lookup-node-row dbis database-name node-id)))
        (unless node-row
          (return-from fetch-node-edges nil))
        (multiple-value-bind (adjacency key)
            (%adjacency-db-and-key dbis node-row predicate incoming)
          (unless adjacency
            (return-from fetch-node-edges nil))
          (let ((edges nil))
            (lmdb:do-db-dup (value adjacency key)
              (multiple-value-bind (neighbor-row edge-row)
                  (%decode-uint64-pair value)
                (declare (ignore neighbor-row))
                (let ((bytes (lmdb:g3t (graph-dbis-edges dbis) edge-row)))
                  (when bytes
                    (push (%decode-document-or-object bytes) edges)))))
            (nreverse edges)))))))

(defun fetch-node-neighbors (database node-id
                             &key
                               (database-name (get-default-graph-db))
                               incoming
                               predicate)
  "Return neighboring NODE objects using fixed-width adjacency records.

After resolving NODE-ID once, traversal stays entirely on uint64/fixed-octet
keyspaces: one DUPSORT scan plus one integer-key node lookup per result. Edge
records are not touched."
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database)
      (let ((node-row (%lookup-node-row dbis database-name node-id)))
        (unless node-row
          (return-from fetch-node-neighbors nil))
        (multiple-value-bind (adjacency key)
            (%adjacency-db-and-key dbis node-row predicate incoming)
          (unless adjacency
            (return-from fetch-node-neighbors nil))
          (let ((neighbors nil))
            (lmdb:do-db-dup (value adjacency key)
              (multiple-value-bind (neighbor-row edge-row)
                  (%decode-uint64-pair value)
                (declare (ignore edge-row))
                (let ((bytes (lmdb:g3t (graph-dbis-nodes dbis) neighbor-row)))
                  (when bytes
                    (push (%decode-document-or-object bytes) neighbors)))))
            (nreverse neighbors)))))))

(defun delete-edge (database edge &key (database-name (get-default-graph-db)))
  "Delete EDGE, its external-id mapping, and all adjacency entries atomically."
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database :write t)
      (let ((edge-row (%lookup-edge-row dbis database-name (edge-id edge))))
        (when edge-row
          (let ((bytes (lmdb:g3t (graph-dbis-edges dbis) edge-row)))
            (when bytes
              (%remove-edge-adjacency
               dbis database-name edge-row
               (%decode-document-or-object bytes))))
          (lmdb:del (graph-dbis-edges dbis) edge-row)
          (lmdb:del (graph-dbis-edge-map dbis)
                    (%external-edge-key database-name (edge-id edge)))))))
  t)
