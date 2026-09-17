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
               (:file "graph-lifecycle")
               (:file "tek9"))
  :in-order-to ((test-op (test-op :tek9-tests))))

(asdf:defsystem :star-git
  :description "Immutable Git-like revision history for StarIntel documents on Tek9."
  :author "nsaspy"
  :license "MIT"
  :version "0.1.0"
  :pathname "star-git"
  :serial t
  :depends-on (#:tek9 #:ironclad #:babel)
  :components ((:file "package")
               (:file "repository")))
