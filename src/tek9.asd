(asdf:defsystem :tek9
  :description "Fast embedded Common Lisp document and graph database on LMDB."
  :author "nsaspy"
  :license "MIT"
  :version "0.2.0"
  :serial t
  :depends-on (#:alexandria #:serapeum #:jsown #:cl-ulid #:lmdb #:cl-conspack)
  :components ((:file "package")
               (:file "objects")
               (:file "documents")
               (:file "indexes")
               (:file "graphs")
               (:file "views")
               (:file "query")
               (:file "tek9")))
