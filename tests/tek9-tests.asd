(asdf:defsystem :tek9-tests
  :description "Tests for Tek9"
  :author "nsaspy"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:tek9 #:fiveam)
  :components ((:file "package")
               (:file "create-db")
               (:file "test-documents")))
