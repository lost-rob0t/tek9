(in-package :tek9)

(defun %select (database
                &key
                  (database-name +main-name+)
                  (where (lambda (key document)
                           (declare (ignore key))
                           (not (null (doc-value document)))))
                  (limit 0)
                  (offset 0))
  "Full-scan fallback for predicates without an indexable constraint."
  (let ((db (%main-db database database-name))
        (results nil)
        (matched 0)
        (emitted 0))
    (with-database (database)
      (lmdb:do-db (key bytes db)
        (let ((document (%decode-document bytes)))
          (when (funcall where key document)
            (if (< matched offset)
                (incf matched)
                (progn
                  (incf matched)
                  (incf emitted)
                  (push (cons key (doc-value document)) results)
                  (when (and (plusp limit)
                             (>= emitted limit))
                    (return))))))))
    (nreverse results)))

(defmacro select (database &rest args)
  "Compatibility macro around the optimized SELECT implementation."
  `(%select ,database ,@args))

(defun select-primary-range (database start
                             &key
                               end
                               (database-name +main-name+)
                               (limit 0))
  "Scan the ordered primary-key B+tree starting at START.

Unlike SELECT, this performs one B+tree seek then sequential cursor movement."
  (let ((db (%main-db database database-name))
        (results nil)
        (emitted 0))
    (with-database (database)
      (lmdb:with-cursor (cursor db)
        (multiple-value-bind (key bytes found)
            (lmdb:cursor-set-range start cursor)
          (loop while found
                while (or (null end) (string<= key end))
                do (push (cons key
                               (doc-value (%decode-document bytes)))
                         results)
                   (incf emitted)
                   (when (and (plusp limit)
                              (>= emitted limit))
                     (return))
                   (multiple-value-setq (key bytes found)
                     (lmdb:cursor-next cursor))))))
    (nreverse results)))

(defun select-index (database index-name key
                     &key
                       (project #'doc-value))
  "Resolve equality through a registered secondary index."
  (mapcar project (index-fetch database index-name key)))

(defun select-index-range (database index-name start
                           &key
                             end
                             (limit 0)
                             (project #'doc-value))
  "Range scan a secondary index with one seek and sequential cursor traversal."
  (let* ((index (or (secondary-index-by-name database index-name)
                    (error "Unknown Tek9 index ~S." index-name)))
         (index-db (index-db index))
         (source (%main-db database (index-source-db index)))
         (start (%coerce-index-key index start))
         (end (and end (%coerce-index-key index end)))
         (results nil)
         (emitted 0))
    (with-database (database)
      (lmdb:with-cursor (cursor index-db)
        (multiple-value-bind (key id found)
            (lmdb:cursor-set-range start cursor)
          (loop while found
                while (or (null end)
                          (etypecase key
                            (string (string<= key end))
                            (integer (<= key end))))
                do (let ((bytes (lmdb:g3t source id)))
                     (when bytes
                       (push (funcall project (%decode-document bytes))
                             results)
                       (incf emitted)))
                   (when (and (plusp limit)
                              (>= emitted limit))
                     (return))
                   (multiple-value-setq (key id found)
                     (lmdb:cursor-next cursor))))))
    (nreverse results)))
