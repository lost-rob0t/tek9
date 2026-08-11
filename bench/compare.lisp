;;;; Comparative Tek9 microbenchmarks for storage-engine regressions.
;;;; Measures reference usage shapes against the v0.2 fast paths on the same build.

(in-package :cl-user)

(defpackage :tek9-bench-compare
  (:use :cl :tek9)
  (:export :run))

(in-package :tek9-bench-compare)

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

(defun report-pair (label slow fast)
  (format t "~&~A~%  reference: ~,6Fs~%  fast:      ~,6Fs~%  speedup:   ~,2Fx~%"
          label slow fast (if (plusp fast) (/ slow fast) most-positive-fixnum)))

(defun report-normalized-pair (label slow slow-count fast fast-count)
  (let ((slow-per (/ slow slow-count))
        (fast-per (/ fast fast-count)))
    (report-pair label slow-per fast-per)
    (format t "  samples:   ~D reference / ~D fast~%" slow-count fast-count)))

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
           (report-pair "full-durability write transactions" slow fast))
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
           (let ((slow
                   (elapsed-seconds
                    (lambda ()
                      (dolist (id ids)
                        (fetch db id)))))
                 (fast
                   (elapsed-seconds
                    (lambda ()
                      (fetch-bulk db ids)))))
             (report-pair "point reads: one snapshot each vs one snapshot/cursor" slow fast)))
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
             (report-normalized-pair
              "selective equality: decoded scan vs DUPSORT index"
              slow slow-queries fast fast-queries)))
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
             (report-pair "secondary-index rebuild: random insert vs sorted append"
                          slow fast)))
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
             (report-pair "indexed initial load: row-maintained vs bulk-built"
                          slow fast)))
      (close-database row-maintained)
      (close-database bulk-built))))

(defun graph-neighbors-via-edge-objects (db graph-name center)
  "Reference traversal: materialize edges, then resolve their target nodes."
  (let ((ids (mapcar #'edge-target
                     (fetch-node-edges db center
                                       :database-name graph-name
                                       :predicate "knows"))))
    (fetch-bulk-nodes db ids :database-name graph-name)))

(defun benchmark-graph-neighbors (fanout queries)
  (let* ((db (fresh-db #P"/tmp/tek9-bench-graph/"))
         (graph-name "fanout")
         (center "center")
         (leaves
           (loop for i below fanout
                 collect (make-instance 'node
                                        :id (format nil "node-~8,'0d" i))))
         (edges
           (loop for node in leaves
                 for i from 0
                 collect (make-instance 'edge
                                        :id (format nil "edge-~8,'0d" i)
                                        :source center
                                        :predicate "knows"
                                        :target (node-id node)))))
    (unwind-protect
         (progn
           (put-nodes db
                      (cons (make-instance 'node :id center) leaves)
                      :database-name graph-name)
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
             (report-normalized-pair
              "graph neighbors: edge materialization vs direct adjacency payload"
              slow queries fast queries)))
      (close-database db))))

(defun run (&key
              (write-count 1000)
              (read-count 5000)
              (query-count 5000)
              (queries 250)
              (index-build-count 20000)
              (buckets 1000)
              (graph-fanout 2000)
              (graph-queries 50))
  (format t "~&Tek9 comparative microbenchmarks (durability=:FULL)~%")
  (benchmark-write-batching write-count)
  (benchmark-bulk-read read-count)
  (benchmark-secondary-index query-count queries buckets)
  (benchmark-index-rebuild index-build-count buckets)
  (benchmark-indexed-initial-load index-build-count buckets)
  (benchmark-graph-neighbors graph-fanout graph-queries)
  (values))
