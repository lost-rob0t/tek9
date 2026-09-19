(in-package :tek9)

(defparameter +ingest-worker-max-request-bytes+ (* 16 1024 1024))
(defparameter +ingest-worker-max-records+ 4096)

(defun %json-object-p (value)
  (and (consp value) (eq (first value) :obj)))

(defun %json-required (object key)
  (multiple-value-bind (value found)
      (jsown:val-safe object key)
    (unless found
      (error "Missing JSON field ~S." key))
    value))

(defun %json-optional (object key default)
  (multiple-value-bind (value found)
      (jsown:val-safe object key)
    (if found value default)))

(defun %worker-array (value name)
  (unless (listp value)
    (error "~A must be a JSON array." name))
  (when (> (length value) +ingest-worker-max-records+)
    (error "~A exceeds the ~D-record request bound."
           name +ingest-worker-max-records+))
  value)

(defun %worker-token (value name &key (max-length 512))
  (unless (and (stringp value)
               (< 0 (length value))
               (<= (length value) max-length))
    (error "~A must be a bounded non-empty string." name))
  value)

(defun %worker-integer (value name &key zero-allowed)
  (unless (and (integerp value)
               (if zero-allowed (>= value 0) (> value 0)))
    (error "~A must be a ~:[positive~;non-negative~] integer."
           name zero-allowed))
  value)

(defun %worker-watermark (value)
  (unless (or (null value)
              (stringp value)
              (integerp value))
    (error "watermark must be null, a string, or an integer."))
  value)

(defun %worker-documents (value)
  (loop for item in (%worker-array value "documents")
        do (unless (%json-object-p item)
             (error "document entry must be an object."))
        collect
        (new-document
         :id (%worker-token (%json-required item "id")
                            "document id")
         :value (%json-required item "value"))))

(defun %worker-nodes (value)
  (loop for item in (%worker-array value "nodes")
        do (unless (%json-object-p item)
             (error "node entry must be an object."))
        collect
        (make-instance
         'node
         :id (%worker-token (%json-required item "id") "node id")
         :props (%json-required item "props"))))

(defun %worker-edges (value)
  (loop for item in (%worker-array value "edges")
        do (unless (%json-object-p item)
             (error "edge entry must be an object."))
        collect
        (make-instance
         'edge
         :id (%worker-token (%json-required item "id") "edge id")
         :source (%worker-token (%json-required item "source")
                                "edge source")
         :predicate (%worker-token (%json-required item "predicate")
                                   "edge predicate")
         :target (%worker-token (%json-required item "target")
                                "edge target"))))

(defun %json-ok (&rest specs)
  (jsown:to-json
   (cons :obj
         (cons (cons "ok" t)
               specs))))

(defun %json-error (code &rest specs)
  (jsown:to-json
   (cons :obj
         (cons (cons "ok" :false)
               (cons (cons "code" code)
                     specs)))))

(defun %checkpoint-response (checkpoint)
  (let ((generation (if checkpoint
                        (or (getf checkpoint :generation) 0)
                        0))
        (watermark (if checkpoint
                       (getf checkpoint :watermark)
                       nil)))
    (%json-ok
     (cons "generation" generation)
     (cons "watermark" (if (null watermark) :null watermark)))))

(defun %apply-ingest-json-object (database request)
  (unless (%json-object-p request)
    (error "Request must be a JSON object."))
  (let ((operation (%worker-token (%json-required request "op")
                                  "operation"
                                  :max-length 64)))
    (cond
      ((string= operation "status")
       (let ((source-id
               (%worker-token (%json-required request "source_id")
                              "source_id")))
         (%checkpoint-response
          (fetch-ingest-checkpoint database source-id))))

      ((string= operation "apply_batch")
       (let* ((source-id
                (%worker-token (%json-required request "source_id")
                               "source_id"))
              (generation
                (%worker-integer (%json-required request "generation")
                                 "generation"))
              (expected-generation
                (%worker-integer
                 (%json-required request "expected_generation")
                 "expected_generation"
                 :zero-allowed t))
              (documents
                (%worker-documents
                 (%json-optional request "documents" nil)))
              (nodes
                (%worker-nodes
                 (%json-optional request "nodes" nil)))
              (edges
                (%worker-edges
                 (%json-optional request "edges" nil)))
              (document-database-name
                (%worker-token
                 (%json-optional request "database_name" +main-name+)
                 "database_name"))
              (graph-name
                (%worker-token
                 (%json-optional request
                                 "graph_name"
                                 (get-default-graph-db))
                 "graph_name"))
              (watermark
                (%worker-watermark
                 (%json-optional request "watermark" nil))))
         (let ((checkpoint
                 (apply-ingest-batch
                  database source-id generation
                  :expected-generation expected-generation
                  :documents documents
                  :nodes nodes
                  :edges edges
                  :document-database-name document-database-name
                  :graph-name graph-name
                  :watermark watermark)))
           (%checkpoint-response checkpoint))))

      (t
       (%json-error "unsupported_operation")))))

(defun apply-ingest-json-request (database request)
  "Apply one bounded JSON request and return one JSON response string.

This is a fixed command surface, not a Lisp reader or evaluator. The only
operations are APPLY_BATCH and STATUS."
  (unless (stringp request)
    (error "request must be a string."))
  (when (> (length request) +ingest-worker-max-request-bytes+)
    (return-from apply-ingest-json-request
      (%json-error "request_too_large")))
  (handler-case
      (%apply-ingest-json-object database (jsown:parse request))
    (ingest-generation-conflict (condition)
      (%json-error
       "generation_conflict"
       (cons "expected_generation"
             (ingest-generation-conflict-expected-generation condition))
       (cons "actual_generation"
             (ingest-generation-conflict-actual-generation condition))
       (cons "requested_generation"
             (ingest-generation-conflict-requested-generation condition))))
    (error ()
      (%json-error "invalid_request"))))

(defun run-ingest-jsonl
    (database &key (input *standard-input*) (output *standard-output*))
  "Serve fixed ingest requests from newline-delimited JSON until INPUT EOF."
  (loop for line = (read-line input nil nil)
        while line
        do (write-line (apply-ingest-json-request database line) output)
           (finish-output output))
  nil)

(defun run-ingest-worker
    (path &key
            (name "tek9-ingest")
            (max-size (* 64 1024 1024 1024))
            (input *standard-input*)
            (output *standard-output*))
  "Open one Tek9 database at PATH and serve JSONL requests until EOF."
  (let ((database
          (open-database
           (new-database name
                         :path (uiop:ensure-directory-pathname path)
                         :max-size max-size
                         :durability :full))))
    (unwind-protect
         (run-ingest-jsonl database :input input :output output)
      (close-database database))))
