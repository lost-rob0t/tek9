(in-package :tek9)

(defparameter +ingest-checkpoint-db+ "ingest/checkpoints")

(define-condition ingest-generation-conflict (error)
  ((source-id
    :initarg :source-id
    :reader ingest-generation-conflict-source-id)
   (expected-generation
    :initarg :expected-generation
    :reader ingest-generation-conflict-expected-generation)
   (actual-generation
    :initarg :actual-generation
    :reader ingest-generation-conflict-actual-generation)
   (requested-generation
    :initarg :requested-generation
    :reader ingest-generation-conflict-requested-generation))
  (:report
   (lambda (condition stream)
     (format stream
             "Ingest generation conflict for ~S: expected current ~D, actual ~D, requested ~D."
             (ingest-generation-conflict-source-id condition)
             (ingest-generation-conflict-expected-generation condition)
             (ingest-generation-conflict-actual-generation condition)
             (ingest-generation-conflict-requested-generation condition)))))

(defun %validate-ingest-source-id (source-id)
  (unless (and (stringp source-id)
               (< 0 (length source-id) 513)
               (every (lambda (character)
                        (let ((code (char-code character)))
                          (and (>= code #x21)
                               (/= code #x7f))))
                      source-id))
    (error "Ingest source id must be 1..512 visible non-whitespace characters."))
  source-id)

(defun %validate-ingest-generation (value name &key zero-allowed)
  (unless (and (integerp value)
               (if zero-allowed (>= value 0) (> value 0)))
    (error "~A must be a ~:[positive~;non-negative~] integer."
           name zero-allowed))
  value)

(defun fetch-ingest-checkpoint (database source-id)
  "Return SOURCE-ID's checkpoint plist or NIL.

Checkpoint records live in a dedicated document DB so importers can advance a
source watermark atomically with ordinary documents and graph mutations."
  (fetch* database
          (%validate-ingest-source-id source-id)
          :database-name +ingest-checkpoint-db+))

(defun ingest-generation (database source-id)
  "Return SOURCE-ID's committed generation, or zero when it has never committed."
  (let ((checkpoint (fetch-ingest-checkpoint database source-id)))
    (if checkpoint
        (or (getf checkpoint :generation) 0)
        0)))

(defun apply-ingest-batch
    (database source-id generation
     &key
       (expected-generation 0)
       documents
       nodes
       edges
       (document-database-name +main-name+)
       (graph-name (get-default-graph-db))
       watermark)
  "Atomically apply one external-source batch and advance its checkpoint.

EXPECTED-GENERATION fences stale writers. GENERATION must be strictly greater
than the currently committed generation. DOCUMENTS, NODES, and EDGES use Tek9's
normal batch APIs and therefore reuse this outer LMDB write transaction.
WATERMARK is opaque caller-owned progress data persisted only after every
mutation in the batch succeeds.

The source adapter owns external schemas (Navidrome, import files, APIs, etc.).
Tek9 deliberately stores only the normalized records it is given."
  (%validate-ingest-source-id source-id)
  (%validate-ingest-generation generation "generation")
  (%validate-ingest-generation expected-generation
                               "expected-generation"
                               :zero-allowed t)
  (unless (stringp document-database-name)
    (error "document-database-name must be a string."))
  (unless (stringp graph-name)
    (error "graph-name must be a string."))

  (with-write-transaction
      (database
       :database-names
       (remove-duplicates
        (list document-database-name +ingest-checkpoint-db+)
        :test #'string=))
    (let ((actual-generation (ingest-generation database source-id)))
      (unless (and (= actual-generation expected-generation)
                   (> generation actual-generation))
        (error 'ingest-generation-conflict
               :source-id source-id
               :expected-generation expected-generation
               :actual-generation actual-generation
               :requested-generation generation))

      (when documents
        (put-bulk database documents
                  :database-name document-database-name))
      (when nodes
        (put-nodes database nodes :database-name graph-name))
      (when edges
        (put-edges database edges :database-name graph-name))

      (put database
           (new-document
            :id source-id
            :value (list :source-id source-id
                         :generation generation
                         :watermark watermark
                         :committed-at (get-universal-time)))
           :database-name +ingest-checkpoint-db+)))

  (fetch-ingest-checkpoint database source-id))
