;;;; Comparative Tek9 microbenchmarks for storage-engine regressions.
;;;; Measures reference usage shapes against the v0.2 fast paths on the same build.

(in-package :cl-user)

(defpackage :tek9-bench-compare
  (:use :cl :tek9)
  (:export :run :run-and-write))

(in-package :tek9-bench-compare)

(defvar *results* nil)

(defun fresh-db (path &key (map-size (* 2 1024 1024 1024)))
  (when (uiop:directory-exists-p path)
    (uiop:delete-directory-tree path :validate t))
  (open-database
   (new-database "bench"
                 :path path
                 :max-size map-size
                 :durability :full)))

(defun elapsed-seconds (thunk)
  (let ((start (get-internal-real-time)))
    (funcall thunk)
    (/ (- (get-internal-real-time) start)
       (float internal-time-units-per-second))))

(defun record-result (name reference fast &key reference-samples fast-samples)
  (let* ((reference-per (/ reference (or reference-samples 1)))
         (fast-per (/ fast (or fast-samples 1)))
         (speedup (if (plusp fast-per)
                      (/ reference-per fast-per)
                      most-positive-fixnum)))
    (push (list :name name
                :reference-seconds reference-per
                :fast-seconds fast-per
                :speedup speedup
                :reference-samples (or reference-samples 1)
                :fast-samples (or fast-samples 1))
          *results*)
    (format t "~&~A~%  reference: ~,6Fs~%  fast:      ~,6Fs~%  speedup:   ~,2Fx~%"
            name reference-per fast-per speedup)
    (when (or reference-samples fast-samples)
      (format t "  samples:   ~D reference / ~D fast~%"
              (or reference-samples 1)
              (or fast-samples 1)))
    speedup))

(defun make-documents (count &key buckets)
  (loop for i below count
        collect
        (new-document
         :id (format nil "~12,'0d" i)
         :changed t
         :value (list :n i
                      :bucket (and buckets
                                   (format nil "b~4,'0d" (mod i buckets)))
                      :payload "tek9-benchmark"))))

(defun register-bucket-index (db)
  (register-index
   db
   "bucket"
   (lambda (document)
     (getf (doc-value document) :bucket))))

(defun benchmark-write-batching (count)
  (let ((individual (fresh-db #P"/tmp/tek9-bench-individual/"))
        (batched (fresh-db #P"/tmp/tek9-bench-batched/"))
        (documents (make-documents count)))
    (unwind-protect
         (let ((slow
                 (elapsed-seconds
                  (lambda ()
                    (dolist (document documents)
                      (put individual document)))))
               (fast
                 (elapsed-seconds
                  (lambda ()
                    (put-bulk batched documents :sorted t)))))
           (record-result "full-durability-write-batching" slow fast))
      (close-database individual)
      (close-database batched))))

(defun benchmark-bulk-read (count)
  (let* ((db (fresh-db #P"/tmp/tek9-bench-read/"))
         (documents (make-documents count))
         (ids (mapcar #'doc-id documents)))
    (unwind-protect
         (progn
           (put-bulk db documents :sorted t)
           (fetch db (first ids))
           ;; Both paths materialize the same (id . document) result shape.
           ;; The reference pays one read transaction per key; FETCH-BULK
           ;; shares one snapshot across all direct MDB_GET calls.
           (let ((slow
                   (elapsed-seconds
                    (lambda ()
                      (loop for id in ids
                            collect (cons id (fetch db id))))))
                 (fast
                   (elapsed-seconds
                    (lambda ()
                      (fetch-bulk db ids)))))
             (record-result "bulk-point-read-single-snapshot" slow fast)))
      (close-database db))))

(defun benchmark-secondary-index (count slow-queries buckets)
  (let* ((db (fresh-db #P"/tmp/tek9-bench-index/"))
         (documents (make-documents count :buckets buckets))
         (needle (format nil "b~4,'0d" (floor buckets 2)))
         (fast-queries (max 10000 (* slow-queries 40))))
    (unwind-protect
         (progn
           (put-bulk db documents :sorted t)
           (register-bucket-index db)
           (rebuild-index db "bucket")
           (tek9::%select db
                          :where (lambda (key document)
                                   (declare (ignore key))
                                   (equal needle
                                          (getf (doc-value document) :bucket))))
           (select-index db "bucket" needle)
           (let ((slow
                   (elapsed-seconds
                    (lambda ()
                      (loop repeat slow-queries
                            do (tek9::%select
                                db
                                :where (lambda (key document)
                                         (declare (ignore key))
                                         (equal needle
                                                (getf (doc-value document)
                                                      :bucket))))))))
                 (fast
                   (elapsed-seconds
                    (lambda ()
                      (loop repeat fast-queries
                            do (select-index db "bucket" needle))))))
             (record-result "selective-equality-index"
                            slow fast
                            :reference-samples slow-queries
                            :fast-samples fast-queries)))
      (close-database db))))

(defun benchmark-index-rebuild (count buckets)
  (let ((streaming (fresh-db #P"/tmp/tek9-bench-rebuild-streaming/"))
        (sequential (fresh-db #P"/tmp/tek9-bench-rebuild-sequential/"))
        (documents (make-documents count :buckets buckets)))
    (unwind-protect
         (progn
           (put-bulk streaming documents :sorted t)
           (put-bulk sequential documents :sorted t)
           (register-bucket-index streaming)
           (register-bucket-index sequential)
           (let ((slow (elapsed-seconds
                        (lambda () (rebuild-index streaming "bucket"))))
                 (fast (elapsed-seconds
                        (lambda () (rebuild-index-fast sequential "bucket")))))
             (record-result "secondary-index-rebuild" slow fast)))
      (close-database streaming)
      (close-database sequential))))

(defun benchmark-indexed-initial-load (count buckets)
  (let ((row-maintained (fresh-db #P"/tmp/tek9-bench-indexed-load-row/"))
        (bulk-built (fresh-db #P"/tmp/tek9-bench-indexed-load-bulk/"))
        (documents (make-documents count :buckets buckets)))
    (unwind-protect
         (progn
           (register-bucket-index row-maintained)
           (register-bucket-index bulk-built)
           (let ((slow
                   (elapsed-seconds
                    (lambda ()
                      (put-bulk row-maintained documents :sorted t))))
                 (fast
                   (elapsed-seconds
                    (lambda ()
                      (bulk-load bulk-built documents)))))
             (record-result "indexed-initial-load" slow fast)))
      (close-database row-maintained)
      (close-database bulk-built))))

(defun make-fanout-graph (fanout)
  (let* ((center "center")
         (leaves
           (loop for i below fanout
                 collect (make-instance 'node
                                        :id (format nil "node-~8,'0d" i))))
         (nodes (cons (make-instance 'node :id center) leaves))
         (edges
           (loop for node in leaves
                 for i from 0
                 collect (make-instance 'edge
                                        :id (format nil "edge-~8,'0d" i)
                                        :source center
                                        :predicate "knows"
                                        :target (node-id node)))))
    (values center nodes edges)))

(defun benchmark-graph-edge-ingest (fanout)
  "Measure transaction-local row-resolution caches on fan-out edge ingest."
  (let ((uncached (fresh-db #P"/tmp/tek9-bench-graph-ingest-uncached/"))
        (cached (fresh-db #P"/tmp/tek9-bench-graph-ingest-cached/"))
        (graph-name "ingest"))
    (multiple-value-bind (center nodes edges)
        (make-fanout-graph fanout)
      (declare (ignore center))
      (unwind-protect
           (progn
             (put-nodes uncached nodes :database-name graph-name)
             (put-nodes cached nodes :database-name graph-name)
             (let ((slow
                     (elapsed-seconds
                      (lambda ()
                        (tek9::%put-edges-uncached
                         uncached edges :database-name graph-name))))
                   (fast
                     (elapsed-seconds
                      (lambda ()
                        (put-edges cached edges :database-name graph-name)))))
               (record-result "graph-edge-batch-row-cache" slow fast)))
        (close-database uncached)
        (close-database cached)))))

(defun graph-neighbors-via-edge-objects (db graph-name center)
  "Reference traversal: materialize edges, then resolve their target nodes."
  (let ((ids (mapcar #'edge-target
                     (fetch-node-edges db center
                                       :database-name graph-name
                                       :predicate "knows"))))
    (fetch-bulk-nodes db ids :database-name graph-name)))

(defun benchmark-graph-neighbors (fanout queries)
  (let* ((db (fresh-db #P"/tmp/tek9-bench-graph/"))
         (graph-name "fanout"))
    (multiple-value-bind (center nodes edges)
        (make-fanout-graph fanout)
      (unwind-protect
           (progn
             (put-nodes db nodes :database-name graph-name)
             (put-edges db edges :database-name graph-name)
             (graph-neighbors-via-edge-objects db graph-name center)
             (fetch-node-neighbors db center
                                   :database-name graph-name
                                   :predicate "knows")
             (let ((slow
                     (elapsed-seconds
                      (lambda ()
                        (loop repeat queries
                              do (graph-neighbors-via-edge-objects
                                  db graph-name center)))))
                   (fast
                     (elapsed-seconds
                      (lambda ()
                        (loop repeat queries
                              do (fetch-node-neighbors
                                  db center
                                  :database-name graph-name
                                  :predicate "knows"))))))
               (record-result "graph-neighbor-direct-adjacency"
                              slow fast
                              :reference-samples queries
                              :fast-samples queries)))
        (close-database db)))))

(defun run (&key
              (write-count 1000)
              (read-count 5000)
              (query-count 5000)
              (queries 250)
              (index-build-count 20000)
              (buckets 1000)
              (graph-ingest-fanout 5000)
              (graph-fanout 2000)
              (graph-queries 50))
  (setf *results* nil)
  (format t "~&Tek9 comparative microbenchmarks (durability=:FULL)~%")
  (benchmark-write-batching write-count)
  (benchmark-bulk-read read-count)
  (benchmark-secondary-index query-count queries buckets)
  (benchmark-index-rebuild index-build-count buckets)
  (benchmark-indexed-initial-load index-build-count buckets)
  (benchmark-graph-edge-ingest graph-ingest-fanout)
  (benchmark-graph-neighbors graph-fanout graph-queries)
  (nreverse *results*))

(defun json-escape (string)
  (with-output-to-string (out)
    (loop for character across string
          do (case character
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (write-char character out))))))

(defun write-results-json (pathname results)
  (ensure-directories-exist pathname)
  (let ((commit (or (uiop:getenv "BENCHMARK_COMMIT")
                    (uiop:getenv "GITHUB_SHA")
                    "unknown")))
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "{~%  \"schema_version\": 1,~%  \"commit\": \"~A\",~%  \"durability\": \"full\",~%  \"metrics\": {~%"
              (json-escape commit))
      (loop for result in results
            for first = t then nil
            do (unless first
                 (format stream ",~%"))
               (format stream
                       "    \"~A\": {\"reference_seconds\": ~,9F, \"fast_seconds\": ~,9F, \"speedup\": ~,9F, \"reference_samples\": ~D, \"fast_samples\": ~D}"
                       (json-escape (getf result :name))
                       (coerce (getf result :reference-seconds) 'double-float)
                       (coerce (getf result :fast-seconds) 'double-float)
                       (coerce (getf result :speedup) 'double-float)
                       (getf result :reference-samples)
                       (getf result :fast-samples))
            finally (format stream "~%  }~%}~%")))))

(defun run-and-write (&optional
                        (pathname
                          (or (uiop:getenv "BENCHMARK_JSON")
                              "bench/results/current.json")))
  (let ((results (run)))
    (write-results-json pathname results)
    results))
