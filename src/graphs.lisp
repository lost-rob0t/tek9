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

(defun %reserve-row-ids (meta key count)
  "Reserve COUNT consecutive row IDs with one counter read and one counter write.

Returns the first reserved row ID and the exclusive upper bound. A zero COUNT
performs no LMDB write."
  (if (zerop count)
      (values 0 0)
      (let* ((current (or (lmdb:g3t meta key) 0))
             (first (1+ current))
             (limit (+ first count)))
        (lmdb:put meta key (1- limit))
        (values first limit))))

(defun %lookup-node-row (dbis graph-name external-id)
  (lmdb:g3t (graph-dbis-node-map dbis)
            (%external-node-key graph-name external-id)))

(defun %lookup-node-row-cached (dbis graph-name external-id cache)
  "Resolve EXTERNAL-ID, using CACHE only for the lifetime of the current txn."
  (if (null cache)
      (%lookup-node-row dbis graph-name external-id)
      (multiple-value-bind (row present)
          (gethash external-id cache)
        (if present
            row
            (let ((resolved (%lookup-node-row dbis graph-name external-id)))
              (when resolved
                (setf (gethash external-id cache) resolved))
              resolved)))))

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

(defun %lookup-predicate-row-cached (dbis predicate cache)
  (let ((key (princ-to-string predicate)))
    (if (null cache)
        (lmdb:g3t (graph-dbis-predicate-map dbis) key)
        (multiple-value-bind (row present)
            (gethash key cache)
          (if present
              row
              (let ((resolved (lmdb:g3t (graph-dbis-predicate-map dbis) key)))
                (when resolved
                  (setf (gethash key cache) resolved))
                resolved))))))

(defun %ensure-predicate-row (dbis predicate)
  (let ((key (princ-to-string predicate)))
    (or (lmdb:g3t (graph-dbis-predicate-map dbis) key)
        (let ((row (%next-row-id
                    (graph-dbis-meta dbis)
                    +next-predicate-id-key+)))
          (lmdb:put (graph-dbis-predicate-map dbis) key row)
          row))))

(defun %ensure-predicate-row-cached (dbis predicate cache)
  (let ((key (princ-to-string predicate)))
    (if (null cache)
        (%ensure-predicate-row dbis predicate)
        (multiple-value-bind (row present)
            (gethash key cache)
          (if (and present row)
              row
              (let ((resolved (%ensure-predicate-row dbis predicate)))
                (setf (gethash key cache) resolved)
                resolved))))))

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

(defun %put-nodes-per-row-counter
    (database nodes &key (database-name (get-default-graph-db)))
  "Reference implementation: allocate each new node row through the meta counter."
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database :write t)
      (dolist (node nodes)
        (let ((row (%ensure-node-row dbis database-name (node-id node))))
          (lmdb:put (graph-dbis-nodes dbis) row (%* node)))))
    nodes))

(defun put-nodes (database nodes &key (database-name (get-default-graph-db)) sorted)
  "Persist NODES in one transaction using one row-ID reservation per batch.

Existing rows keep their IDs. New rows are discovered first, then a contiguous
uint64 block is reserved with one meta read/write instead of mutating the row
counter once per node. SORTED is retained for API compatibility."
  (declare (ignore sorted))
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database :write t)
      (let ((pending nil))
        (dolist (node nodes)
          (let ((row (%lookup-node-row dbis database-name (node-id node))))
            (if row
                (lmdb:put (graph-dbis-nodes dbis) row (%* node))
                (push node pending))))
        (multiple-value-bind (row limit)
            (%reserve-row-ids (graph-dbis-meta dbis)
                              +next-node-id-key+
                              (length pending))
          (declare (ignore limit))
          (dolist (node (nreverse pending))
            (lmdb:put (graph-dbis-node-map dbis)
                      (%external-node-key database-name (node-id node))
                      row)
            (lmdb:put (graph-dbis-nodes dbis) row (%* node))
            (incf row)))))
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

(defun %put-adjacency-value (db key value)
  (lmdb:put db key value :dupdata nil :key-exists-error-p nil))

(defun %delete-adjacency-value (db key value)
  (lmdb:del db key :value value))

(defun %remove-edge-adjacency (dbis graph-name edge-row edge
                               &key node-cache predicate-cache)
  "Remove EDGE's current adjacency entries in the active write transaction."
  (let* ((source-row (%lookup-node-row-cached
                      dbis graph-name (edge-source edge) node-cache))
         (target-row (%lookup-node-row-cached
                      dbis graph-name (edge-target edge) node-cache))
         (predicate-row (%lookup-predicate-row-cached
                         dbis (edge-predicate edge) predicate-cache)))
    (when (and source-row target-row)
      (let ((out-value (%adjacency-value target-row edge-row))
            (in-value (%adjacency-value source-row edge-row)))
        (%delete-adjacency-value (graph-dbis-out dbis)
                                 source-row out-value)
        (%delete-adjacency-value (graph-dbis-in dbis)
                                 target-row in-value)
        (when predicate-row
          (%delete-adjacency-value
           (graph-dbis-out-predicate dbis)
           (%predicate-adjacency-key source-row predicate-row)
           out-value)
          (%delete-adjacency-value
           (graph-dbis-in-predicate dbis)
           (%predicate-adjacency-key target-row predicate-row)
           in-value))))))

(defun %write-edge-row (dbis graph-name edge edge-row existing-row-p
                        &key node-cache predicate-cache)
  "Write EDGE to EDGE-ROW and maintain every adjacency index in the active txn."
  (let* ((source-row (%lookup-node-row-cached
                      dbis graph-name (edge-source edge) node-cache))
         (target-row (%lookup-node-row-cached
                      dbis graph-name (edge-target edge) node-cache)))
    (unless source-row
      (error "Unknown source node ~S in graph ~S."
             (edge-source edge) graph-name))
    (unless target-row
      (error "Unknown target node ~S in graph ~S."
             (edge-target edge) graph-name))
    (let ((predicate-row (%ensure-predicate-row-cached
                          dbis (edge-predicate edge) predicate-cache)))
      (when existing-row-p
        (let ((previous-bytes (lmdb:g3t (graph-dbis-edges dbis) edge-row)))
          (when previous-bytes
            (%remove-edge-adjacency
             dbis graph-name edge-row
             (%decode-document-or-object previous-bytes)
             :node-cache node-cache
             :predicate-cache predicate-cache))))
      (let ((out-value (%adjacency-value target-row edge-row))
            (in-value (%adjacency-value source-row edge-row))
            (out-key (%predicate-adjacency-key source-row predicate-row))
            (in-key (%predicate-adjacency-key target-row predicate-row)))
        (lmdb:put (graph-dbis-edges dbis) edge-row (%* edge))
        (%put-adjacency-value (graph-dbis-out dbis)
                              source-row out-value)
        (%put-adjacency-value (graph-dbis-in dbis)
                              target-row in-value)
        (%put-adjacency-value (graph-dbis-out-predicate dbis)
                              out-key out-value)
        (%put-adjacency-value (graph-dbis-in-predicate dbis)
                              in-key in-value))
      edge-row)))

(defun %insert-edge (dbis graph-name edge &key node-cache predicate-cache)
  "Reference single-edge path with per-row counter allocation."
  (let* ((existing-row (%lookup-edge-row dbis graph-name (edge-id edge)))
         (edge-row (or existing-row
                       (%ensure-edge-row dbis graph-name (edge-id edge)))))
    (%write-edge-row dbis graph-name edge edge-row (not (null existing-row))
                     :node-cache node-cache
                     :predicate-cache predicate-cache)))

(defun %put-edges-uncached (database edges
                            &key (database-name (get-default-graph-db)))
  "Reference batch-ingest path used by the benchmark regression suite."
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database :write t)
      (dolist (edge edges)
        (%insert-edge dbis database-name edge)))
    edges))

(defun put-edges (database edges &key (database-name (get-default-graph-db)))
  "Persist EDGES and every adjacency index atomically.

The batch resolves each external edge ID once, reserves one contiguous uint64
row-ID block for all new edges, and updates the edge counter once. Node and
predicate mappings are memoized only for this transaction."
  (let ((dbis (ensure-graph-dbs database))
        (node-cache (make-hash-table :test #'equal))
        (predicate-cache (make-hash-table :test #'equal)))
    (with-database (database :write t)
      (let ((prepared nil)
            (new-count 0))
        (dolist (edge edges)
          (let ((existing-row
                  (%lookup-edge-row dbis database-name (edge-id edge))))
            (unless existing-row
              (incf new-count))
            (push (cons edge existing-row) prepared)))
        (multiple-value-bind (next-row limit)
            (%reserve-row-ids (graph-dbis-meta dbis)
                              +next-edge-id-key+
                              new-count)
          (dolist (entry (nreverse prepared))
            (let* ((edge (car entry))
                   (existing-row (cdr entry))
                   (edge-row
                     (or existing-row
                         (prog1 next-row
                           (lmdb:put (graph-dbis-edge-map dbis)
                                     (%external-edge-key
                                      database-name (edge-id edge))
                                     next-row)
                           (incf next-row)))))
              (%write-edge-row
               dbis database-name edge edge-row (not (null existing-row))
               :node-cache node-cache
               :predicate-cache predicate-cache)))
          (unless (= next-row limit)
            (error "Tek9 edge row reservation invariant failed: ~D != ~D."
                   next-row limit)))))
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
