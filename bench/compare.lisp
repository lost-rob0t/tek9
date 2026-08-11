;;;; Comparative Tek9 microbenchmarks for storage-engine regressions.
;;;; Measures old usage shapes against the v0.2 fast paths on the same build.

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
  (format t "~&~A~%  reference: ~,4Fs~%  fast:      ~,4Fs~%  speedup:   ~,2Fx~%"
          label slow fast (if (plusp fast) (/ slow fast) most-positive-fixnum)))

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
           ;; Warm the mapping before timing transaction/cursor overhead.
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

(defun benchmark-secondary-index (count queries buckets)
  (let* ((db (fresh-db #P"/tmp/tek9-bench-index/"))
         (documents (make-documents count :buckets buckets))
         (needle (format nil "b~4,'0d" (floor buckets 2))))
    (unwind-protect
         (progn
           (put-bulk db documents :sorted t)
           (register-index
            db
            "bucket"
            (lambda (document)
              (getf (doc-value document) :bucket)))
           (rebuild-index db "bucket")
           ;; Warm both code paths.
           (tek9::%select db
                          :where (lambda (key document)
                                   (declare (ignore key))
                                   (equal needle
                                          (getf (doc-value document) :bucket))))
           (index-document-ids db "bucket" needle)
           (let ((slow
                   (elapsed-seconds
                    (lambda ()
                      (dotimes (i queries)
                        (declare (ignore i))
                        (tek9::%select
                         db
                         :where (lambda (key document)
                                  (declare (ignore key))
                                  (equal needle
                                         (getf (doc-value document) :bucket))))))))
                 (fast
                   (elapsed-seconds
                    (lambda ()
                      (dotimes (i queries)
                        (declare (ignore i))
                        (index-document-ids db "bucket" needle))))))
             (report-pair "selective equality: decoded scan vs DUPSORT index" slow fast)))
      (close-database db))))

(defun run (&key
              (write-count 1000)
              (read-count 5000)
              (query-count 5000)
              (queries 250)
              (buckets 1000))
  (format t "~&Tek9 comparative microbenchmarks (durability=:FULL)~%")
  (benchmark-write-batching write-count)
  (benchmark-bulk-read read-count)
  (benchmark-secondary-index query-count queries buckets)
  (values))
