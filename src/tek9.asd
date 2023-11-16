(asdf:defsystem :tek9
  :description "An adventure into LMDB from common lisp."
  :author "nsaspy"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:alexandria  #:serapeum #:jsown #:cl-ulid #:lmdb #:cl-conspack)
  :components ((:file "package")
               (:file "objects")
               (:file "documents")
               (:file "graphs")
               (:file "views")
               (:file "query")
               (:file "tek9")))
