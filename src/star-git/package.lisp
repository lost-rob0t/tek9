(in-package :cl-user)

(uiop:define-package :star-git
  (:use :cl)
  (:import-from :tek9
                :new-database
                :open-database
                :close-database
                :database-db
                :register-index
                :put*
                :fetch*
                :index-fetch
                :doc-value
                :with-read-transaction
                :with-write-transaction
                :map-database)
  (:export
   :repository
   :repository-database
   :open-repository
   :close-repository
   :write-blob
   :read-object
   :read-blob
   :commit-document
   :read-commit
   :document-ref-name
   :resolve-ref
   :update-ref
   :log-document
   :inventory-commits
   :commits-for-blob
   :build-pack
   :import-pack
   :pack-id-for-path
   :read-pack-metadata
   :pack-locations
   :fsck
   :ref-conflict
   :ref-conflict-ref
   :ref-conflict-expected
   :ref-conflict-current
   :missing-object
   :missing-object-id
   :object-integrity-error
   :object-integrity-error-id
   :lzma-codec-error
   :lzma-codec-error-phase
   :lzma-codec-error-code
   :pack-error
   :pack-error-pathname
   :pack-error-reason
   :pack-integrity-error
   :pack-integrity-object-id))
