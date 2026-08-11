(in-package :tek9)

(defparameter +main-name+ "std"
  "Main document database name inside an LMDB environment.")

(defconstant +default-map-size+ (* 16 1024 1024 1024)
  "Default LMDB map size: 16 GiB of virtual address space.")

(defconstant +default-max-dbs+ 64
  "Default number of named LMDB databases reserved for indexes, views, and graph keyspaces.")

(defvar *db* nil "Convenience database variable used by the REPL examples.")

(defclass database ()
  ((path :initarg :path :initform nil :accessor db-path)
   (env :initarg :env :initform nil :accessor db-env)
   (name :initarg :name :initform "" :accessor db-name)
   (open :initform nil :accessor db-open?)
   (handles :initform (make-hash-table :test #'equal) :accessor db-handles)
   (indexes :initform (make-hash-table :test #'equal) :accessor db-indexes)
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
                       (durability :full))
  (uiop:ensure-all-directories-exist (list path))
  (make-instance 'database
                 :name name
                 :path path
                 :size max-size
                 :max-dbs max-dbs
                 :max-readers max-readers
                 :durability durability))

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

(defmethod open-database ((db database) &key max-dbs max-readers)
  "Open DB and pre-open the main document keyspace.

The default durability profile is fully ACID. :METADATA-LAZY keeps atomicity,
consistency and isolation while allowing the last committed transaction to be
lost on a system crash. :NOSYNC is opt-in and intended only for rebuildable data."
  (when (db-is-open-p db)
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
      (database-db db +main-name+
                   :key-encoding :utf-8
                   :value-encoding :octets)
      (setf (db-open? db) t)
      db)))

(defmethod close-database ((db database))
  "Close DB if open and invalidate cached named DB handles."
  (when (db-is-open-p db)
    (lmdb:close-env (db-env db)))
  (setf (db-env db) nil
        (db-open? db) nil)
  (clrhash (db-handles db))
  db)

(defmacro with-database ((database &key (write nil)) &body body)
  "Execute BODY in one LMDB transaction using DATABASE's durability profile.

WITH-TXN commits automatically on normal return and aborts on non-local exit."
  `(multiple-value-bind (sync meta-sync)
       (durability-options (db-durability ,database))
     (lmdb:with-txn (:env (db-env ,database)
                     :write ,write
                     :sync sync
                     :meta-sync meta-sync)
       ,@body)))

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
          :database (lmdb:db-statistics db))))

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
