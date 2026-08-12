;;;; Simple Tek9 microbenchmarks.
;;;; Run after loading :tek9.

(in-package :cl-user)

(defpackage :tek9-bench
  (:use :cl :tek9)
  (:export :run))

(in-package :tek9-bench)

(defun seconds-since (start)
  (/ (- (get-internal-real-time) start)
     (float internal-time-units-per-second)))

(defmacro timed ((label) &body body)
  `(let ((start (get-internal-real-time)))
     (prog1
         (progn ,@body)
       (format t "~&~A: ~,3Fs~%" ,label (seconds-since start)))))

(defun make-documents (count)
  (loop for i below count
        collect
        (new-document
         :id (format nil "~12,'0d" i)
         :value (list :dtype (if (evenp i) "even" "odd")
                      :n i
                      :payload "tek9"))))

(defun run (&key (count 100000) (path #P"/tmp/tek9-bench/"))
  (when (uiop:directory-exists-p path)
    (uiop:delete-directory-tree path :validate t))
  (let* ((db (open-database
              (new-database "bench"
                            :path path
                            :max-size (* 8 1024 1024 1024)
                            :durability :full)))
         (documents (make-documents count)))
    (unwind-protect
         (progn
           (timed ("sorted bulk insert")
             (put-bulk db documents :sorted t))
           (timed ("10000 point reads")
             (dotimes (i (min count 10000))
               (fetch db (format nil "~12,'0d" i))))
           (register-index db
                           "dtype"
                           (lambda (document)
                             (getf (doc-value document) :dtype)))
           (timed ("secondary index rebuild")
             (rebuild-index db "dtype"))
           (timed ("1000 indexed equality queries")
             (dotimes (i 1000)
               (declare (ignore i))
               (index-document-ids db "dtype" "even")))
           (timed ("primary range 10000")
             (select-primary-range db
                                   "000000010000"
                                   :end "000000019999"))
           (format t "~&Stats: ~S~%" (database-stats db)))
      (close-database db))))
