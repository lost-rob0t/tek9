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
               (:file "test-graphs")))
