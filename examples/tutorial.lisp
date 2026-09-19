;;;; Tek9 tutorial. Load TEK9 before loading this file.
;;;; Uses only exported Tek9 APIs and never removes an existing directory.

(defpackage :tek9-tutorial
  (:use :cl)
  (:export :run-tutorial))
(in-package :tek9-tutorial)

(define-condition tutorial-rollback (error) ())

(defun tutorial-index-definitions ()
  (list (tek9:new-index-definition
         "person-type"
         (lambda (document)
           (getf (tek9:doc-value document) :type)))))

(defun open-tutorial-database (path)
  (tek9:open-database
   (tek9:new-database "tutorial"
                      :path path
                      :max-size (* 64 1024 1024)
                      :durability :full
                      :index-definitions (tutorial-index-definitions))))

(defun make-people-view ()
  (let ((view (tek9:new-view "people-by-id" nil)))
    (tek9:define-map view
      (when (equal "person" (getf (tek9:doc-value doc) :type))
        (emit (tek9:doc-id doc) (getf (tek9:doc-value doc) :name))))
    (tek9:define-reduce view
      (length rows))
    view))

(defun write-example-data (database)
  ;; Declare first-use document keyspaces at the OUTERMOST boundary.
  (tek9:with-write-transaction (database :database-names '("metadata"))
    (tek9:put-bulk
     database
     (list (tek9:new-document :id "person:ada"
                              :value '(:type "person" :name "Ada"))
           (tek9:new-document :id "person:grace"
                              :value '(:type "person" :name "Grace"))))
    (tek9:put* database '(:schema-version 1) :id "schema"
               :database-name "metadata")
    (tek9:put-nodes
     database
     (list (make-instance 'tek9:node :id "ada" :props '(:name "Ada"))
           (make-instance 'tek9:node :id "grace" :props '(:name "Grace")))
     :database-name "people")
    (tek9:put-edge
     database
     (make-instance 'tek9:edge :id "ada-knows-grace"
                              :source "ada" :target "grace"
                              :predicate "knows")
     :database-name "people")))

(defun verify-example-data (database)
  (tek9:with-read-transaction (database :database-names '("metadata"))
    (assert (equal "Ada" (getf (tek9:fetch* database "person:ada") :name)))
    (assert (equal '("person:ada" "person:grace")
                   (tek9:index-document-ids database "person-type" "person")))
    (assert (= 2 (length (tek9:select-primary-range
                         database "person:" :end "person:zzzz" :limit 10))))
    (assert (= 1 (getf (tek9:fetch* database "schema"
                                   :database-name "metadata")
                       :schema-version)))
    (assert (equal '("grace")
                   (mapcar #'tek9:node-id
                           (tek9:fetch-node-neighbors
                            database "ada" :database-name "people"
                            :predicate "knows")))))
  t)

(defun demonstrate-rollback (database)
  ;; Catch OUTSIDE the write boundary, never swallow failed mutations inside it.
  (handler-case
      (tek9:with-write-transaction (database)
        (tek9:put* database '(:type "transient" :name "Never") :id "rolled-back")
        (error 'tutorial-rollback))
    (tutorial-rollback () nil))
  (assert (null (tek9:fetch database "rolled-back")))
  ;; This tutorial uses explicit full rebuilds, not the legacy dirty vector.
  (tek9:clear-changes database))

(defun demonstrate-view (database)
  (let ((view (make-people-view)))
    (tek9:add-view database view)
    (tek9:apply-view-to-database database view)
    (assert (= 2 (tek9:reduce-view database view)))
    (multiple-value-bind (value present-p)
        (tek9:view-get database view "person:ada")
      (assert present-p)
      (assert (equal "Ada" value)))
    ;; Full rebuild retracts stale rows; incremental deletion is under audit.
    (tek9:with-write-transaction (database)
      (tek9:put* database '(:type "person" :name "Ada Lovelace")
                 :id "person:ada")
      (tek9:apply-view-to-database database view))
    (assert (equal "Ada Lovelace"
                   (tek9:view-get database view "person:ada")))
    (tek9:clear-changes database)
    (tek9:view-rows database view :limit 10)))

(defun run-tutorial (&key (path #P"/tmp/tek9-tutorial/"))
  "Create a fresh demonstration DB, assert behavior, and verify a fresh reopen.
PATH must not exist. Data is deliberately retained for inspection."
  (check-type path (or pathname string))
  (let ((directory (uiop:ensure-directory-pathname path)))
    ;; Dedicated local demo path only; this is not a concurrent provisioning API.
    (when (probe-file directory)
      (error "Refusing existing tutorial path ~A; choose a fresh directory."
             directory))
    (let ((database nil)
          (rows nil))
      (unwind-protect
           (progn
             (setf database (open-tutorial-database directory))
             (write-example-data database)
             (verify-example-data database)
             (demonstrate-rollback database)
             (setf rows (demonstrate-view database))
             (tek9:close-database database)
             ;; Functions are configuration: supply the same index definitions.
             (setf database (open-tutorial-database directory))
             (assert (equal "Ada Lovelace"
                            (getf (tek9:fetch* database "person:ada") :name)))
             (assert (= 2 (length (tek9:index-document-ids
                                  database "person-type" "person"))))
             (assert (tek9:fetch-edge database "ada-knows-grace"
                                      :database-name "people"))
             (assert (null (tek9:fetch database "rolled-back")))
             (format t "~&Tek9 tutorial passed. Retained database: ~A~%" directory)
             (values directory rows))
        (when database
          (tek9:close-database database))))))
