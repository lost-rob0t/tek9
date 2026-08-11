(in-package :tek9)

(defun make-key-id ()
  "Return a lexicographically sortable ULID string."
  (multiple-value-bind (ulid-str ulid-bytes)
      (ulid:ulid)
    (declare (ignore ulid-bytes))
    ulid-str))

(defclass document ()
  ((id :initform (make-key-id) :type string :initarg :id :accessor doc-id)
   (value :initform nil :initarg :value :accessor doc-value)
   (changed :initform nil :initarg :changed :accessor doc-changed)))

(conspack:defencoding document
  id value changed)

(defun new-document (&rest keys-vals)
  (apply #'make-instance 'document keys-vals))

(defun touch-document (database document)
  "Mark DOCUMENT dirty and append its id to DATABASE's change vector."
  (setf (doc-changed document) t)
  (vector-push-extend (doc-id document) (db-changed database))
  document)

(defun untouch-document (document)
  (setf (doc-changed document) nil)
  document)

(defun %decode-document (bytes)
  (and bytes ($ bytes)))

(defun %main-db (database database-name)
  (database-db database database-name
               :key-encoding :utf-8
               :value-encoding :octets))

(defun put (database document &key (database-name +main-name+))
  "Insert or replace DOCUMENT in one ACID transaction.

Secondary indexes are maintained in the same LMDB transaction as the document."
  (let* ((db (%main-db database database-name))
         (indexes (secondary-indexes-for database database-name))
         (document (touch-document database document)))
    (with-database (database :write t)
      (let ((previous (when indexes
                        (%decode-document (lmdb:g3t db (doc-id document))))))
        (update-secondary-indexes database previous document database-name)
        (lmdb:put db (doc-id document) (%* document))))
    document))

(defun put* (database value &key (id (make-key-id)) (database-name +main-name+))
  (put database
       (new-document :id id :value value :changed t)
       :database-name database-name))

(defun put-bulk (database documents
                 &key
                   (database-name +main-name+)
                   sorted
                   track-changes)
  "Write DOCUMENTS in one LMDB write transaction.

SORTED is a performance hint. Tek9 uses MDB_APPEND only if the first input key
is beyond the physical database's current final key; otherwise it safely falls
back to ordinary insertion. TRACK-CHANGES is opt-in for materialized-view
workflows so large imports do not allocate a second list/vector of every id."
  (let* ((db (%main-db database database-name))
         (indexes (secondary-indexes-for database database-name)))
    (with-database (database :write t)
      (let ((append-p (and sorted
                           documents
                           (%append-safe-p db (doc-id (first documents))))))
        (dolist (document documents)
          (when track-changes
            (touch-document database document))
          (let ((previous (when (and indexes (not append-p))
                            (%decode-document
                             (lmdb:g3t db (doc-id document))))))
            (update-secondary-indexes database previous document database-name)
            (lmdb:put db
                      (doc-id document)
                      (%* document)
                      :append append-p)))))
    documents))

(defun put-bulk* (database documents
                  &key
                    (database-name +main-name+)
                    sorted
                    track-changes)
  (put-bulk database
            (loop for (key value) in documents
                  collect (new-document :id key :value value))
            :database-name database-name
            :sorted sorted
            :track-changes track-changes))

(defun put-json (database json)
  (let* ((json-data (jsown:parse json))
         (id (or (jsown:val-safe json-data "_id")
                 (jsown:val-safe json-data "id")
                 (make-key-id))))
    (put database (new-document :id id :value json-data))))

(defun fetch (database id &key (database-name +main-name+))
  "Fetch a DOCUMENT by primary key in a read-only snapshot."
  (let ((db (%main-db database database-name)))
    (with-database (database)
      (%decode-document (lmdb:g3t db id)))))

(defun fetch* (database id &key (database-name +main-name+))
  (let ((document (fetch database id :database-name database-name)))
    (and document (doc-value document))))

(defun fetch-bulk (database document-ids &key (database-name +main-name+))
  "Fetch many ids in one snapshot while reusing one LMDB cursor.

The result order matches DOCUMENT-IDS. Missing ids are returned with NIL values."
  (let ((db (%main-db database database-name)))
    (with-database (database)
      (lmdb:with-cursor (cursor db)
        (loop for id in document-ids
              collect
              (multiple-value-bind (bytes found)
                  (lmdb:cursor-set-key id cursor)
                (cons id (and found (%decode-document bytes)))))))))

(defun fetch-bulk* (database document-ids &key (database-name +main-name+))
  (loop for (id . document)
          in (fetch-bulk database document-ids :database-name database-name)
        collect (cons id (and document (doc-value document)))))

(defun delete-document (database id &key (database-name +main-name+))
  "Delete ID and its secondary-index entries atomically."
  (let* ((db (%main-db database database-name))
         (indexes (secondary-indexes-for database database-name))
         (deleted nil))
    (with-database (database :write t)
      (let ((previous (%decode-document (lmdb:g3t db id))))
        (when previous
          (when indexes
            (update-secondary-indexes database previous nil database-name))
          (setf deleted (lmdb:del db id)))))
    deleted))
