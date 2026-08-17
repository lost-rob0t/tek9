(in-package :tek9)

(defmethod prepare-transaction-dbis :after ((database database) &key database-names)
  (declare (ignore database-names))
  ;; LMDB's Common Lisp wrapper rejects first-time GET-DB while a transaction
  ;; is active. Graph/v2 uses a fixed physical keyspace, so opening those DBIs
  ;; here is bounded and keeps the layout private from callers.
  (ensure-graph-dbs database)
  ;; Registered views have similarly stable process-local identities.
  (loop for view being the hash-values of (db-views database)
        do (create-view-db database view)))

(defun %string-prefix-p (prefix string)
  (let ((prefix-length (length prefix)))
    (and (<= prefix-length (length string))
         (string= prefix string :end2 prefix-length))))

(defun %decode-key-part-at (key start)
  (let ((colon (position #\: key :start start)))
    (unless colon
      (error "Malformed Tek9 composite graph key ~S." key))
    (let* ((length (parse-integer key :start start :end colon))
           (value-start (1+ colon))
           (value-end (+ value-start length)))
      (when (> value-end (length key))
        (error "Malformed Tek9 composite graph key ~S." key))
      (values (subseq key value-start value-end) value-end))))

(defun %external-id-from-mapping-key (graph-name key)
  (multiple-value-bind (stored-graph next)
      (%decode-key-part-at key 0)
    (unless (string= graph-name stored-graph)
      (error "Graph mapping key ~S belongs to ~S, not ~S."
             key stored-graph graph-name))
    (multiple-value-bind (external-id end)
        (%decode-key-part-at key next)
      (unless (= end (length key))
        (error "Malformed trailing bytes in Tek9 graph key ~S." key))
      external-id)))

(defun %graph-mapping-entries (mapping-db graph-name)
  "Return (EXTERNAL-ID . ROW-ID) entries for GRAPH-NAME in key order.

The cursor seeks to the graph prefix instead of scanning unrelated graph
namespaces. Must be called inside an active transaction."
  (let ((prefix (%key-part graph-name))
        (entries nil))
    (lmdb:with-cursor (cursor mapping-db)
      (multiple-value-bind (key row found)
          (lmdb:cursor-set-range prefix cursor)
        (loop while (and found (%string-prefix-p prefix key))
              do (push (cons (%external-id-from-mapping-key graph-name key) row)
                       entries)
                 (multiple-value-setq (key row found)
                   (lmdb:cursor-next cursor)))))
    (nreverse entries)))

(defun fetch-edge (database id &key (database-name (get-default-graph-db)))
  "Fetch one EDGE by stable external ID from logical graph DATABASE-NAME."
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database)
      (let ((row (%lookup-edge-row dbis database-name id)))
        (and row
             (%decode-document-or-object
              (lmdb:g3t (graph-dbis-edges dbis) row)))))))

(defun fetch-graph-nodes (database database-name)
  "Return every NODE in logical graph DATABASE-NAME, sorted by external ID."
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database)
      (sort
       (loop for (external-id . row)
               in (%graph-mapping-entries (graph-dbis-node-map dbis) database-name)
             for bytes = (lmdb:g3t (graph-dbis-nodes dbis) row)
             when bytes
               collect (%decode-document-or-object bytes)
             else
               do (warn "Tek9 graph ~S node mapping ~S points to missing row ~D."
                        database-name external-id row))
       #'string< :key #'node-id))))

(defun fetch-graph-edges (database database-name)
  "Return every EDGE in logical graph DATABASE-NAME, sorted by external ID."
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database)
      (sort
       (loop for (external-id . row)
               in (%graph-mapping-entries (graph-dbis-edge-map dbis) database-name)
             for bytes = (lmdb:g3t (graph-dbis-edges dbis) row)
             when bytes
               collect (%decode-document-or-object bytes)
             else
               do (warn "Tek9 graph ~S edge mapping ~S points to missing row ~D."
                        database-name external-id row))
       #'string< :key #'edge-id))))

(defun delete-node (database id &key (database-name (get-default-graph-db)))
  "Delete node ID and all of its incident edges atomically.

Every normal and predicate-specific inbound/outbound adjacency record is removed
through DELETE-EDGE before the node row and external mapping are deleted."
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database :write t)
      (let ((row (%lookup-node-row dbis database-name id)))
        (unless row
          (return-from delete-node nil))
        (let ((edge-ids
                (remove-duplicates
                 (append (fetch-node-edge-ids database id
                                              :database-name database-name)
                         (fetch-node-edge-ids database id
                                              :database-name database-name
                                              :incoming t))
                 :test #'string=)))
          (dolist (edge-id edge-ids)
            (let ((edge (fetch-edge database edge-id
                                    :database-name database-name)))
              (when edge
                (delete-edge database edge :database-name database-name)))))
        (lmdb:del (graph-dbis-nodes dbis) row)
        (lmdb:del (graph-dbis-node-map dbis)
                  (%external-node-key database-name id))
        t))))

(defun clear-graph (database database-name)
  "Remove all topology owned by logical graph DATABASE-NAME atomically.

Other graph namespaces, including ones reusing the same external node or edge
IDs, are untouched. Global monotonic row counters and predicate dictionary rows
are intentionally retained."
  (let ((dbis (ensure-graph-dbs database)))
    (with-database (database :write t)
      (let ((edge-entries
              (%graph-mapping-entries (graph-dbis-edge-map dbis) database-name))
            (node-entries
              (%graph-mapping-entries (graph-dbis-node-map dbis) database-name)))
        (dolist (entry edge-entries)
          (let* ((external-id (car entry))
                 (row (cdr entry))
                 (bytes (lmdb:g3t (graph-dbis-edges dbis) row)))
            (when bytes
              (%remove-edge-adjacency
               dbis database-name row (%decode-document-or-object bytes)))
            (lmdb:del (graph-dbis-edges dbis) row)
            (lmdb:del (graph-dbis-edge-map dbis)
                      (%external-edge-key database-name external-id))))
        (dolist (entry node-entries)
          (lmdb:del (graph-dbis-nodes dbis) (cdr entry))
          (lmdb:del (graph-dbis-node-map dbis)
                    (%external-node-key database-name (car entry))))
        t))))
