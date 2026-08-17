(in-package :tek9)

(defparameter +main-name+ "std"
  "Main document database name inside an LMDB environment.")

(defconstant +default-map-size+ (* 16 1024 1024 1024)
  "Default LMDB map size: 16 GiB of virtual address space.")

(defconstant +default-max-dbs+ 64
  "Default number of named LMDB databases reserved for indexes, views, and graph keyspaces.")

(defvar *db* nil "Convenience database variable used by the REPL examples.")

(defclass index-definition ()
  ((name :initarg :name :reader index-definition-name)
   (key-fn :initarg :key-fn :reader index-definition-key-fn)
   (database-name :initarg :database-name
                  :initform +main-name+
                  :reader index-definition-database-name)
   (key-type :initarg :key-type
             :initform :string
             :reader index-definition-key-type)
   (unique :initarg :unique
           :initform nil
           :reader index-definition-unique-p)
   (multi-valued :initarg :multi-valued
                 :initform nil
                 :reader index-definition-multi-valued-p))
  (:documentation
   "Declarative secondary-index configuration reapplied when a database opens."))

(defun new-index-definition (name key-fn
                             &key
                               (database-name +main-name+)
                               (key-type :string)
                               unique
                               multi-valued)
  "Create a reusable declarative INDEX-DEFINITION.

INDEX-DEFINITION objects contain Lisp extractor functions, so they are process
configuration rather than serialized database metadata. OPEN-DATABASE reapplies
them to the durable LMDB index DBs on every process start."
  (check-type name string)
  (check-type key-fn function)
  (check-type database-name string)
  (check-type key-type (member :string :uint64))
  (make-instance 'index-definition
                 :name name
                 :key-fn key-fn
                 :database-name database-name
                 :key-type key-type
                 :unique unique
                 :multi-valued multi-valued))

(defparameter *index-definitions* nil
  "Default index registry consulted by OPEN-DATABASE.

Set this to a list of INDEX-DEFINITION objects for application-wide automatic
index rehydration. A DATABASE can override the global registry through
NEW-DATABASE or OPEN-DATABASE :INDEX-DEFINITIONS. Explicit NIL disables
automatic registration for that database.")

(defclass database ()
  ((path :initarg :path :initform nil :accessor db-path)
   (env :initarg :env :initform nil :accessor db-env)
   (name :initarg :name :initform "" :accessor db-name)
   (open :initform nil :accessor db-open?)
   (handles :initform (make-hash-table :test #'equal) :accessor db-handles)
   (indexes :initform (make-hash-table :test #'equal) :accessor db-indexes)
   (index-definitions :initarg :index-definitions
                      :initform :default
                      :accessor db-index-definitions)
   (views :initarg :views :accessor db-views :initform (dict))
   (document-count :initarg :count :initform 0 :accessor db-count)
   (changes :initform (make-array 64 :adjustable t :fill-pointer 0)
            :accessor db-changed)
   (size :initform +default-map-size+ :initarg :size :accessor db-max-size :type integer)
   (max-dbs :initform +default-max-dbs+ :initarg :max-dbs :accessor db-max-dbs :type integer)
   (max-readers :initform 126 :initarg :max-readers :accessor db-max-readers :type integer)
   (durability :initform :full :initarg :durability :accessor db-durability))
  (:documentation
   "Tek9 database handle.

LMDB owns transaction isolation and durability. Tek9 caches named DB handles here
because GET-DB performs a linear lookup through opened DB handles."))

(defmethod db-is-open-p ((db database))
  (and (db-open? db)
       (db-env db)
       (lmdb:open-env-p (db-env db))))

(defun durability-options (durability)
  "Return LMDB SYNC and META-SYNC values for DURABILITY."
  (ecase durability
    (:full (values t t))
    (:metadata-lazy (values t nil))
    (:nosync (values nil nil))))

(defun new-database (name
                     &key
                       (path (uiop:parse-unix-namestring "./tek9-database/"))
                       (max-size +default-map-size+)
                       (max-dbs +default-max-dbs+)
                       (max-readers 126)
                       (durability :full)
                       (index-definitions :default))
  "Create a Tek9 database handle.

INDEX-DEFINITIONS defaults to :DEFAULT, meaning OPEN-DATABASE reads the current
value of *INDEX-DEFINITIONS*. Pass a list to use database-specific definitions,
or NIL to disable automatic index registration."
  (uiop:ensure-all-directories-exist (list path))
  (make-instance 'database
                 :name name
                 :path path
                 :size max-size
                 :max-dbs max-dbs
                 :max-readers max-readers
                 :durability durability
                 :index-definitions index-definitions))

(defun database-db (database name
                     &key
                       (if-does-not-exist :create)
                       (key-encoding :utf-8)
                       (value-encoding :octets)
                       integer-key
                       reverse-key
                       dupsort
                       integer-dup
                       reverse-dup
                       dupfixed)
  "Return a cached LMDB named database handle.

The first call for NAME fixes LMDB creation flags such as DUPSORT. Named DB
handles are cached to avoid repeated wrapper-level linear handle lookup."
  (or (gethash name (db-handles database))
      (setf (gethash name (db-handles database))
            (lmdb:get-db name
                         :env (db-env database)
                         :if-does-not-exist if-does-not-exist
                         :key-encoding key-encoding
                         :value-encoding value-encoding
                         :integer-key integer-key
                         :reverse-key reverse-key
                         :dupsort dupsort
                         :integer-dup integer-dup
                         :reverse-dup reverse-dup
                         :dupfixed dupfixed))))

(defgeneric register-index-definition (database definition)
  (:documentation
   "Register one declarative INDEX-DEFINITION against an open DATABASE."))

(defun %effective-index-definitions (database)
  (let ((configured (db-index-definitions database)))
    (if (eq configured :default)
        *index-definitions*
        configured)))

(defun %register-configured-indexes (database)
  "Rehydrate DATABASE's process-local index objects from declarative config."
  (dolist (definition (%effective-index-definitions database) database)
    (register-index-definition database definition)))

(defmethod open-database ((db database)
                          &key
                            max-dbs
                            max-readers
                            (index-definitions nil index-definitions-supplied-p))
  "Open DB, pre-open the main keyspace, and rehydrate configured indexes.

The default durability profile is fully ACID. :METADATA-LAZY keeps atomicity,
consistency and isolation while allowing the last committed transaction to be
lost on a system crash. :NOSYNC is opt-in and intended only for rebuildable data.

INDEX-DEFINITIONS overrides the registry stored on DB. When no override is
provided, a DB configured with :DEFAULT reads *INDEX-DEFINITIONS* at open time."
  (when index-definitions-supplied-p
    (setf (db-index-definitions db) index-definitions))
  (when (db-is-open-p db)
    (when index-definitions-supplied-p
      (clrhash (db-indexes db))
      (%register-configured-indexes db))
    (return-from open-database db))
  (multiple-value-bind (sync meta-sync)
      (durability-options (db-durability db))
    (let ((env (lmdb:open-env (db-path db)
                              :if-does-not-exist :create
                              :max-dbs (or max-dbs (db-max-dbs db))
                              :max-readers (or max-readers (db-max-readers db))
                              :map-size (db-max-size db)
                              :synchronized t
                              :sync sync
                              :meta-sync meta-sync)))
      (setf (db-env db) env)
      (clrhash (db-handles db))
      (clrhash (db-indexes db))
      (database-db db +main-name+
                   :key-encoding :utf-8
                   :value-encoding :octets)
      (setf (db-open? db) t)
      (handler-case
          (progn
            (%register-configured-indexes db)
            db)
        (error (condition)
          (close-database db)
          (error condition))))))

(defmethod close-database ((db database))
  "Close DB and invalidate cached LMDB and secondary-index handles."
  (when (db-is-open-p db)
    (lmdb:close-env (db-env db)))
  (setf (db-env db) nil
        (db-open? db) nil)
  (clrhash (db-handles db))
  (clrhash (db-indexes db))
  db)

(define-condition transaction-mode-error (error)
  ((database :initarg :database :reader transaction-mode-error-database)
   (requested-mode :initarg :requested-mode
                   :reader transaction-mode-error-requested-mode)
   (active-mode :initarg :active-mode
                :reader transaction-mode-error-active-mode))
  (:report (lambda (condition stream)
             (format stream
                     "Cannot start Tek9 ~A work inside an active ~A transaction on ~A."
                     (transaction-mode-error-requested-mode condition)
                     (transaction-mode-error-active-mode condition)
                     (db-name (transaction-mode-error-database condition))))))

(defvar *transaction-database* nil
  "DATABASE whose Tek9 transaction boundary is active in this dynamic extent.")

(defvar *transaction-mode* nil
  "Mode of the active Tek9 transaction boundary: :READ or :WRITE.")

(defgeneric prepare-transaction-dbis (database &key database-names)
  (:documentation
   "Open DBIs required by an explicit Tek9 transaction before LMDB begins it.

The LMDB wrapper forbids first-time GET-DB calls inside an active transaction.
Methods may therefore pre-open fixed engine keyspaces. DATABASE-NAMES declares
additional document keyspaces the caller intends to use."))

(defmethod prepare-transaction-dbis ((database database) &key database-names)
  (database-db database +main-name+
               :key-encoding :utf-8
               :value-encoding :octets)
  (dolist (name database-names database)
    (database-db database name
                 :key-encoding :utf-8
                 :value-encoding :octets)))

(defun call-with-database-transaction (database write function)
  "Call FUNCTION in a compatible Tek9 transaction on DATABASE.

Standalone operations create one LMDB transaction. When called within an
existing Tek9 transaction for the same DATABASE, compatible work reuses that
transaction directly instead of creating an LMDB child transaction."
  (check-type function function)
  (if (eq database *transaction-database*)
      (progn
        (when (and write (eq *transaction-mode* :read))
          (error 'transaction-mode-error
                 :database database
                 :requested-mode :write
                 :active-mode :read))
        (funcall function))
      (multiple-value-bind (sync meta-sync)
          (durability-options (db-durability database))
        (lmdb:with-txn (:env (db-env database)
                        :write write
                        :sync sync
                        :meta-sync meta-sync)
          (let ((*transaction-database* database)
                (*transaction-mode* (if write :write :read)))
            (funcall function))))))

(defmacro with-database ((database &key (write nil)) &body body)
  "Execute BODY in a Tek9 transaction, reusing a compatible active boundary.

On a normal standalone call this creates one LMDB transaction using DATABASE's
durability profile. Inside an explicit Tek9 transaction for the same DATABASE,
BODY participates directly in the active transaction."
  (let ((database-var (gensym "DATABASE")))
    `(let ((,database-var ,database))
       (call-with-database-transaction
        ,database-var ,write
        (lambda () ,@body)))))

(defun call-with-transaction (database function mode &key database-names)
  "Call FUNCTION in one explicit Tek9 MODE transaction.

MODE is :READ or :WRITE. Fixed engine DBIs and DATABASE-NAMES are opened before
the outer LMDB transaction begins. Nested compatible Tek9 transactions on the
same DATABASE reuse the active boundary; callers must declare any first-use
custom DATABASE-NAMES at the outermost boundary."
  (check-type function function)
  (check-type mode (member :read :write))
  (unless (eq database *transaction-database*)
    (prepare-transaction-dbis database :database-names database-names))
  (call-with-database-transaction database (eq mode :write) function))

(defmacro with-read-transaction ((database &key database-names) &body body)
  "Execute BODY in one composable read transaction on DATABASE."
  `(call-with-transaction ,database
                          (lambda () ,@body)
                          :read
                          :database-names ,database-names))

(defmacro with-write-transaction ((database &key database-names) &body body)
  "Execute BODY in one composable write transaction on DATABASE.

All normal Tek9 document, index, graph, and view operations on DATABASE reuse
this transaction. Non-local exit aborts the entire boundary."
  `(call-with-transaction ,database
                          (lambda () ,@body)
                          :write
                          :database-names ,database-names))

(defun %ordered-key< (left right)
  (etypecase left
    (string
     (check-type right string)
     (string< left right))
    (integer
     (check-type right integer)
     (< left right))))

(defun %append-safe-p (db first-key)
  "Return true when FIRST-KEY is strictly after DB's current final key.

Must be called inside an active transaction. This turns Tek9's sorted-load flag
into a safe optimization hint instead of blindly asserting MDB_APPEND."
  (lmdb:with-cursor (cursor db)
    (multiple-value-bind (last-key last-value found)
        (lmdb:cursor-last cursor)
      (declare (ignore last-value))
      (or (not found)
          (%ordered-key< last-key first-key)))))

(defun database-stats (database &key (database-name +main-name+))
  "Return environment and named-database statistics without scanning data."
  (let ((db (database-db database database-name)))
    (list :environment (lmdb:env-info (db-env database))
          :database (with-database (database)
                      (lmdb:db-statistics db)))))

(defun clear-changes (database)
  (setf (fill-pointer (db-changed database)) 0)
  database)

(defun map-database (database
                     &key
                       (map-fn #'list)
                       (database-name +main-name+))
  "Map MAP-FN over raw keys and decoded values in one read snapshot."
  (let ((db (database-db database database-name)))
    (with-database (database)
      (lmdb:do-db (key value db)
        (funcall map-fn key ($ value))))))

(defgeneric secondary-indexes-for (database database-name)
  (:documentation "Return registered secondary indexes for DATABASE-NAME."))

(defgeneric update-secondary-indexes
    (database previous-document document database-name)
  (:documentation
   "Update secondary indexes for a document mutation inside the active write transaction."))

(defun %* (entry)
  "Encode ENTRY to Conspack octets."
  (cpk:encode entry))

(defun $ (entry)
  "Decode Conspack octets ENTRY."
  (cpk:decode entry))
