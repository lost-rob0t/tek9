(asdf:defsystem :tek9
  :description "Fast embedded Common Lisp document and graph database on LMDB."
  :author "nsaspy"
  :license "MIT"
  :version "0.2.0"
  :serial t
  :depends-on (#:alexandria #:bordeaux-threads #:serapeum #:jsown #:lmdb #:cl-conspack)
  :components ((:file "package")
               (:file "objects")
               (:file "documents")
               (:file "indexes")
               (:file "graphs")
               (:file "views")
               (:file "query")
               (:file "tek9"))
  :in-order-to ((test-op (test-op :tek9-tests))))
