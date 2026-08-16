(in-package :tek9-tests)
(in-suite :tek9-tests)

(test view-rebuild-and-incremental-update
  (let ((db (setup-db #P"/tmp/test-tek9-views/")))
    (unwind-protect
         (let ((view (new-view "people-by-id" nil)))
           ;; These exported macros must bind symbols read in the caller's package.
           (define-map view
             (when (string= "person" (getf (doc-value doc) :dtype))
               (emit (doc-id doc) (getf (doc-value doc) :name))))
           (define-reduce view
             (length rows))
           (add-view db view)

           (put-bulk db
                     (list (new-document :id "p1"
                                         :value '(:dtype "person" :name "Ada"))
                           (new-document :id "p3"
                                         :value '(:dtype "person" :name "Lin"))
                           (new-document :id "o1"
                                         :value '(:dtype "organization" :name "ACME"))))
           (apply-view-to-database db view)

           ;; Normal readers no longer reach through Tek9 into raw LMDB.
           (multiple-value-bind (value found-p)
               (view-get db view "p1")
             (is found-p)
             (is (string= "Ada" value)))
           (multiple-value-bind (value found-p)
               (view-get db view "o1" :missing)
             (is (not found-p))
             (is (eq :missing value)))

           (is (equal '(#1=("p1" . "Ada") ("p3" . "Lin"))
                      (view-rows db view)))
           (is (equal '(#1#)
                      (view-rows db view :start "p1" :end "p3")))
           (is (equal '(("p3" . "Lin"))
                      (view-rows db view :start "p3" :limit 1)))

           (let (visited)
             (is (= 2 (map-view db view
                                (lambda (key value)
                                  (push (cons key value) visited)))))
             (is (equal '(("p1" . "Ada") ("p3" . "Lin"))
                        (nreverse visited))))
           (is (= 2 (reduce-view db view)))
           (is (= 1 (reduce-view db view :start "p3")))

           ;; A rebuild must clear rows that no longer match the mapper.
           (put db (new-document :id "p1"
                                 :value '(:dtype "organization" :name "Ada")))
           (apply-view-to-database db view)
           (multiple-value-bind (value found-p)
               (view-get db view "p1")
             (declare (ignore value))
             (is (not found-p)))

           ;; WITH-VIEWS applies touched document ids and then clears the change vector.
           (with-views db (list view)
             (put db (new-document :id "p2"
                                   :value '(:dtype "person" :name "Grace"))))
           (multiple-value-bind (value found-p)
               (view-get db view "p2")
             (is found-p)
             (is (string= "Grace" value)))
           (is (zerop (length (db-changed db))))

           (is (delete-view db view))
           (multiple-value-bind (value found-p)
               (view-get db view "p2")
             (declare (ignore value))
             (is (not found-p))))
      (close-database db))))

(test view-reducer-missing-condition
  (let ((db (setup-db #P"/tmp/test-tek9-view-no-reducer/")))
    (unwind-protect
         (let ((view (new-view "no-reducer" nil)))
           (define-map view
             (emit (doc-id doc) (doc-value doc)))
           (add-view db view)
           (put db (new-document :id "x" :value "value"))
           (apply-view-to-database db view)
           (signals view-reducer-missing
             (reduce-view db view)))
      (close-database db))))
