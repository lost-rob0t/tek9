(in-package :tek9)

;; 1. Iter over the database
;; 2. compare the value/id to the where cluase
;; 3. Sort by ordering-by


(defmacro compare ((key val expression) &body form)
  `(if (funcall ,expression ,key ($ ,val))
       ,@form))

(export 'select)
;; Select A document by a function set in where that returns T for a document
(defmacro select (database &key (database-name +main-name+) (where '(lambda (key doc) (not (null (doc-value doc))))) (limit 0))
  `(let ((db (lmdb:get-db ,database-name :env (db-env ,database)))
         (results nil))
     (with-database (,database :write nil)
       (lmdb:do-db (key val db)
         (when (not (= 0 ,limit))
           (if (= ,limit (length results))
               (return)))
         (if (funcall ,where key (doc-value ($ val)))
             (push (cons key (doc-value ($ val))) results))))


     results))

(defmacro update (database value &key (database-name +main-name+) (where '(lambda (key doc) (not (null (doc-value doc))))) (order-by nil) (limit 0))
  `(let ((db (lmdb:get-db ,database-name :env (db-env ,database)))
         (results nil))
     (with-database (,database :write t)
       (lmdb:do-db (key val db)
         (if (funcall ,where key ($ val))
             (lmdb:put db key (%* ,value)))

         (when ,order-by
           (funcall ,order-by key value))))
     results))
