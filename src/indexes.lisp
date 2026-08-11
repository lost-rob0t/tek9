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

(defun clear-index (database name)
  "Remove every key from INDEX in one write transaction."
  (let* ((index (or (secondary-index-by-name database name)
                    (error "Unknown Tek9 index ~S." name)))
         (db (index-db index))
         (keys nil))
    (with-database (database :write t)
      (lmdb:do-db (key value db :nodup t)
        (declare (ignore value))
        (push key keys))
      (dolist (key keys)
        (lmdb:del db key)))
    index))

(defun rebuild-index (database name)
  "Rebuild INDEX from its source documents in one transaction."
  (let* ((index (or (secondary-index-by-name database name)
                    (error "Unknown Tek9 index ~S." name)))
         (source (%main-db database (index-source-db index)))
         (target (index-db index))
         (keys nil))
    (with-database (database :write t)
      (lmdb:do-db (key value target :nodup t)
        (declare (ignore value))
        (push key keys))
      (dolist (key keys)
        (lmdb:del target key))
      (lmdb:do-db (key bytes source)
        (declare (ignore key))
        (%insert-index-values index (%decode-document bytes))))
    index))

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
