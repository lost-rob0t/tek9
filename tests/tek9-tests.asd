(asdf:defsystem :tek9-tests
  :description "Tests for Tek9"
  :author "nsaspy"
  :license "MIT"
  :version "0.2.0"
  :serial t
  :depends-on (#:tek9 #:fiveam)
  :components ((:file "package")
               (:file "create-db")
               (:file "test-documents")
               (:file "test-indexes")
               (:file "test-graphs")
               (:file "test-transactions")
               (:file "test-graph-lifecycle")
               (:file "test-views")
               (:file "test-persistence"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (let ((results (uiop:symbol-call :fiveam :run :tek9-tests)))
               (uiop:symbol-call :fiveam :explain! results)
               (unless (uiop:symbol-call :fiveam :results-status results)
                 (error "Tek9 test suite failed.")))))
