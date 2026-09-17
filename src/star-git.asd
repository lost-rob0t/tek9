(asdf:defsystem :star-git
  :description "Immutable Git-like revision history for StarIntel documents on Tek9."
  :author "nsaspy"
  :license "MIT"
  :version "0.1.0"
  :pathname "star-git"
  :serial t
  :depends-on (#:tek9 #:ironclad #:babel #:cffi)
  :components ((:file "package")
               (:file "repository")
               (:file "snapshots")
               (:file "compression")
               (:file "packs")
               (:file "commit-v2")
               (:file "custody")
               (:file "random-access")))
