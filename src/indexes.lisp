(in-package :tek9)

(defclass secondary-index ()
  ((name :initarg :name :reader index-name)
   (source-db :initarg :source-db :reader index-source-db)
   (key-fn :initarg :key-fn :reader index-key-fn)
   (key-type :initarg :key-type :initform :string :reader index-key-type)
   (unique :initarg :unique :initform nil :reader index-unique-p)
   (multi-valued :initarg :multi-valued :initform nil :reader index-multi-valued-p)
   (db :initarg :db :reader index-db))
  (:documentation
   "A secondary LMDB index mapping an extracted key to document ids."))

(defun %index-db-name (source-db index-name)
  (format nil "idx/~a/~a" source-db index-name))

(defun %index-key-options (key-type)
  (ecase key-type
    (:string (list :key-encoding :utf-8))
    (:uint64 (list :key-encoding :uint64 :integer-key t))))

(defun %coerce-index-key (index value)
  (ecase (index-key-type index)
    (:string
     (etypecase value
       (string value)
       (symbol (symbol-name value))))
    (:uint64
     (check-type value (integer 0 #.(1- (expt 2 64))))
     value)))

(defun %index-values (index document)
  (if (null document)
      nil
      (let ((value (funcall (index-key-fn index) document)))
        (cond
          ((null value) nil)
          ((index-multi-valued-p index) value)
          (t (list value))))))

(defun %index-pair< (left right)
  "Order (INDEX-KEY . DOCUMENT-ID) pairs for LMDB append loading."
  (let ((left-key (car left))
        (right-key (car right)))
    (if (equal left-key right-key)
        (string< (cdr left) (cdr right))
        (%ordered-key< left-key right-key))))

(defun %index-pairs-for-documents (index documents)
  "Materialize and sort index pairs for DOCUMENTS.

This is intentionally an in-memory fast-build primitive. Use REBUILD-INDEX for
the low-memory streaming path when the corpus is too large to sort in RAM."
  (sort
   (loop for document in documents
         append
         (loop for value in (%index-values index document)
               collect (cons (%coerce-index-key index value)
                             (doc-id document))))
   #'%index-pair<))

(defun secondary-index-by-name (database name)
  (gethash name (db-indexes database)))

(defmethod secondary-indexes-for ((database database) database-name)
  (loop for index being the hash-values of (db-indexes database)
        when (string= (index-source-db index) database-name)
          collect index))

(defun register-index (database name key-fn
                       &key
                         (database-name +main-name+)
                         (key-type :string)
                         unique
                         multi-valued
                         rebuild)
  "Register and optionally rebuild a durable secondary index.

KEY-FN receives a DOCUMENT. :STRING and non-negative :UINT64 keyspaces are
supported. Non-unique indexes use LMDB DUPSORT so one key maps directly to
sorted document ids without storing per-key Lisp lists."
  (let* ((db-name (%index-db-name database-name name))
         (key-options (%index-key-options key-type))
         (index-db
           (apply #'database-db
                  database
                  db-name
                  :value-encoding :utf-8
                  :dupsort (not unique)
                  key-options))
         (index (make-instance 'secondary-index
                               :name name
                               :source-db database-name
                               :key-fn key-fn
                               :key-type key-type
                               :unique unique
                               :multi-valued multi-valued
                               :db index-db)))
    (setf (gethash name (db-indexes database)) index)
    (when rebuild
      (rebuild-index database name))
    index))

(defun unregister-index (database name)
  "Remove an index definition from the current Tek9 process.

The durable LMDB named database is intentionally retained."
  (remhash name (db-indexes database)))

(defun %remove-index-values (index document)
  (dolist (value (%index-values index document))
    (lmdb:del (index-db index)
              (%coerce-index-key index value)
              :value (doc-id document))))

(defun %insert-index-values (index document)
  (dolist (value (%index-values index document))
    (let ((key (%coerce-index-key index value)))
      (if (index-unique-p index)
          (lmdb:put (index-db index)
                    key
                    (doc-id document)
                    :overwrite nil)
          (lmdb:put (index-db index)
                    key
                    (doc-id document)
                    :dupdata nil
                    :key-exists-error-p nil)))))

(defmethod update-secondary-indexes
    ((database database) previous-document document database-name)
  (dolist (index (secondary-indexes-for database database-name))
    (when previous-document
      (%remove-index-values index previous-document))
    (when document
      (%insert-index-values index document))))

(defun %clear-index-in-transaction (index)
  "Clear INDEX while already inside a Tek9 write transaction."
  (let ((keys nil)
        (db (index-db index)))
    (lmdb:do-db (key value db :nodup t)
      (progn value)
      (push key keys))
    (dolist (key keys)
      (lmdb:del db key))))

(defun %write-sorted-index-pairs (index pairs)
  "Append pre-sorted PAIRS to an empty INDEX inside the active write txn."
  (let ((db (index-db index)))
    (dolist (pair pairs)
      (if (index-unique-p index)
          (lmdb:put db
                    (car pair)
                    (cdr pair)
                    :overwrite nil
                    :append t)
          (lmdb:put db
                    (car pair)
                    (cdr pair)
                    :dupdata nil
                    :append t
                    :append-dup t
                    :key-exists-error-p nil)))))

(defun clear-index (database name)
  "Remove every key from INDEX in one write transaction."
  (let ((index (or (secondary-index-by-name database name)
                   (error "Unknown Tek9 index ~S." name))))
    (with-database (database :write t)
      (%clear-index-in-transaction index))
    index))

(defun rebuild-index (database name)
  "Rebuild INDEX with bounded memory and ordinary LMDB insertion.

This is the conservative path for very large corpora. REBUILD-INDEX-FAST uses
more RAM but sorts once and feeds LMDB sequentially with APPEND/APPEND-DUP."
  (let* ((index (or (secondary-index-by-name database name)
                    (error "Unknown Tek9 index ~S." name)))
         (source (%main-db database (index-source-db index))))
    (with-database (database :write t)
      (%clear-index-in-transaction index)
      (lmdb:do-db (key bytes source)
        (progn key)
        (%insert-index-values index (%decode-document bytes))))
    index))

(defun rebuild-index-fast (database name)
  "Rebuild INDEX by sorting extracted pairs and append-loading the B+tree.

This minimizes B+tree comparisons and random page work. It intentionally holds
all index pairs in memory, so use REBUILD-INDEX for corpora where that memory
tradeoff is unacceptable. Source mutation is excluded for the duration by the
single LMDB write transaction; readers remain non-blocking."
  (let* ((index (or (secondary-index-by-name database name)
                    (error "Unknown Tek9 index ~S." name)))
         (source (%main-db database (index-source-db index))))
    (with-database (database :write t)
      (let ((pairs nil))
        (lmdb:do-db (key bytes source)
          (progn key)
          (let ((document (%decode-document bytes)))
            (dolist (value (%index-values index document))
              (push (cons (%coerce-index-key index value)
                          (doc-id document))
                    pairs))))
        (setf pairs (sort pairs #'%index-pair<))
        (%clear-index-in-transaction index)
        (%write-sorted-index-pairs index pairs)))
    index))

(defun %db-empty-p (db)
  "Return true when DB has no records. Must run inside an active transaction."
  (lmdb:with-cursor (cursor db)
    (multiple-value-bind (key value found)
        (lmdb:cursor-first cursor)
      (declare (ignore key value))
      (not found))))

(defun bulk-load (database documents
                  &key
                    (database-name +main-name+)
                    track-changes)
  "Fast, atomic initial load of DOCUMENTS and every registered secondary index.

BULK-LOAD requires an empty source database. Documents are sorted by primary
key once, index pairs are extracted and sorted in memory before the write
transaction, then the source and index B+trees are populated sequentially with
LMDB APPEND/APPEND-DUP. This is the preferred initial-ingest path when the
working set fits in RAM.

The entire source+index load commits as one LMDB transaction. If any unique
constraint or append invariant fails, nothing becomes visible."
  (let* ((source (%main-db database database-name))
         (documents (sort (copy-list documents) #'string< :key #'doc-id))
         (indexes (secondary-indexes-for database database-name))
         (index-builds
           (loop for index in indexes
                 collect (cons index
                               (%index-pairs-for-documents index documents)))))
    (with-database (database :write t)
      (unless (%db-empty-p source)
        (error "Tek9 BULK-LOAD requires empty source database ~S."
               database-name))
      (dolist (build index-builds)
        (%clear-index-in-transaction (car build)))
      (dolist (document documents)
        (when track-changes
          (touch-document database document))
        (lmdb:put source (doc-id document) (%* document) :append t))
      (dolist (build index-builds)
        (%write-sorted-index-pairs (car build) (cdr build))))
    documents))

(defun index-document-ids (database name key)
  "Return document ids matching KEY using a direct LMDB index lookup."
  (let* ((index (or (secondary-index-by-name database name)
                    (error "Unknown Tek9 index ~S." name)))
         (db (index-db index))
         (key (%coerce-index-key index key)))
    (with-database (database)
      (if (index-unique-p index)
          (let ((id (lmdb:g3t db key)))
            (and id (list id)))
          (let ((ids nil))
            (lmdb:do-db-dup (id db key)
              (push id ids))
            (nreverse ids))))))

(defun index-fetch (database name key)
  "Fetch documents matching INDEX=KEY in one snapshot."
  (let* ((index (or (secondary-index-by-name database name)
                    (error "Unknown Tek9 index ~S." name)))
         (source (%main-db database (index-source-db index)))
         (ids-db (index-db index))
         (key (%coerce-index-key index key))
         (results nil))
    (with-database (database)
      (labels ((collect-id (id)
                 (let ((bytes (lmdb:g3t source id)))
                   (when bytes
                     (push (%decode-document bytes) results)))))
        (if (index-unique-p index)
            (let ((id (lmdb:g3t ids-db key)))
              (when id
                (collect-id id)))
            (lmdb:do-db-dup (id ids-db key)
              (collect-id id))))
      (nreverse results))))
