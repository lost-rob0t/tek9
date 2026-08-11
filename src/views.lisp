(in-package :tek9)

(defvar +throw-view-errors+ t
  "When true, signal VIEW-ERROR when a view mapper fails.")

(define-condition view-error (error)
  ((view-map :initarg :map :reader view-error-map)
   (view-reduce :initarg :reduce :reader view-error-reduce)
   (cause :initarg :cause :reader view-error-cause))
  (:report (lambda (condition stream)
             (format stream "Tek9 view mapper failed: ~A"
                     (view-error-cause condition)))))

(defclass database-view ()
  ((name :initform "" :type string :accessor view-name :initarg :name)
   (map-fn :initform nil :accessor view-map :initarg :map)
   (reduce-fn :initform nil :accessor view-reduce :initarg :reduce)))

(defmethod add-view ((db database) (view database-view))
  (setf (gethash (view-name view) (db-views db)) view)
  view)

(defun new-view (name map-fn &optional reduce-fn)
  (make-instance 'database-view
                 :map map-fn
                 :reduce reduce-fn
                 :name name))

(defmacro define-map (view &body body)
  "Install a mapper on VIEW while binding caller-visible DOC and EMIT symbols.

The historical macro introduced TEK9-internal DOC/EMIT bindings, which meant a
caller in another package read different symbols in BODY. Resolve the actual
symbols present in BODY so normal exported-macro use works across packages."
  (labels ((find-named-symbol (name form)
             (cond
               ((symbolp form)
                (and (string= (symbol-name form) name) form))
               ((consp form)
                (or (find-named-symbol name (car form))
                    (find-named-symbol name (cdr form)))))))
    (let ((doc-symbol (or (find-named-symbol "DOC" body)
                          (gensym "DOC")))
          (emit-symbol (or (find-named-symbol "EMIT" body)
                           (gensym "EMIT")))
          (result-symbol (gensym "RESULT")))
      `(setf (view-map ,view)
             (lambda (,doc-symbol)
               (let ((,result-symbol nil))
                 (labels ((,emit-symbol (key value)
                            (push (cons key value) ,result-symbol)))
                   ,@body)
                 (nreverse ,result-symbol)))))))

(defmacro define-reduce (view &body body)
  "Install a reducer on VIEW while binding the caller-visible ROWS symbol."
  (labels ((find-named-symbol (name form)
             (cond
               ((symbolp form)
                (and (string= (symbol-name form) name) form))
               ((consp form)
                (or (find-named-symbol name (car form))
                    (find-named-symbol name (cdr form)))))))
    (let ((rows-symbol (or (find-named-symbol "ROWS" body)
                           (gensym "ROWS"))))
      `(setf (view-reduce ,view)
             (lambda (,rows-symbol)
               (declare (ignorable ,rows-symbol))
               ,@body)))))

(defun create-view-db (database view)
  (database-db database
               (format nil "view/~a" (view-name view))
               :key-encoding :utf-8
               :value-encoding :octets))

(defun clear-view (database view)
  (let ((db (create-view-db database view))
        (keys nil))
    (with-database (database :write t)
      (lmdb:do-db (key value db :nodup t)
        (progn value)
        (push key keys))
      (dolist (key keys)
        (lmdb:del db key))))
  view)

(defun delete-view (database view)
  "Stop tracking VIEW and clear its durable rows.

The named LMDB DBI is retained because closing/deleting open DBIs is unsafe."
  (clear-view database view)
  (remhash (view-name view) (db-views database))
  t)

(defun insert-results (database view results)
  (let ((db (create-view-db database view)))
    (with-database (database :write t)
      (dolist (row results)
        (destructuring-bind (key . value) row
          (lmdb:put db key (%* value)))))
    results))

(defun apply-view-to-database (database view)
  "Rebuild VIEW in a single write transaction."
  (let ((source (%main-db database +main-name+))
        (target (create-view-db database view))
        (keys nil))
    (with-database (database :write t)
      (lmdb:do-db (key value target :nodup t)
        (progn value)
        (push key keys))
      (dolist (key keys)
        (lmdb:del target key))
      (lmdb:do-db (key bytes source)
        (progn key)
        (handler-case
            (dolist (row (funcall (view-map view) (%decode-document bytes)))
              (lmdb:put target (car row) (%* (cdr row))))
          (error (cause)
            (when +throw-view-errors+
              (error 'view-error
                     :map (view-map view)
                     :reduce (view-reduce view)
                     :cause cause))))))
    view))

(defun apply-view (database view doc-ids)
  "Incrementally apply VIEW to DOC-IDS in one write transaction."
  (let ((source (%main-db database +main-name+))
        (target (create-view-db database view)))
    (with-database (database :write t)
      (dolist (id doc-ids)
        (let ((bytes (lmdb:g3t source id)))
          (when bytes
            (dolist (row (funcall (view-map view) (%decode-document bytes)))
              (lmdb:put target (car row) (%* (cdr row))))))))
    view))

(defmacro with-views (database views &body body)
  "Execute BODY, then incrementally update VIEWS from Tek9's change vector."
  `(prog1
       (progn ,@body)
     (dolist (view ,views)
       (apply-view ,database view (coerce (db-changed ,database) 'list)))
     (clear-changes ,database)))
