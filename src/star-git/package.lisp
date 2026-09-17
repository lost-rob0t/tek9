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
   :commit-format-version
   :commit-provenance
   :normalize-commit-provenance
   :find-commit-by-couch-rev
   :document-ref-name
   :resolve-ref
   :update-ref
   :log-document
   :inventory-commits
   :commits-for-blob
   ;; immutable snapshot trees / tags
   :write-tree
   :read-tree
   :dataset-snapshot-ref-name
   :snapshot-dataset-day
   :update-object-ref
   :restore-ref-from-tree
   :tag-ref-name
   :write-tag
   :read-tag
   ;; packs
   :build-pack
   :import-pack
   :pack-id-for-path
   :read-pack-metadata
   :pack-locations
   :fsck
   ;; conditions
   :ref-conflict
   :ref-conflict-ref
   :ref-conflict-expected
   :ref-conflict-current
   :missing-object
   :missing-object-id
   :object-integrity-error
   :object-integrity-error-id
   :invalid-provenance
   :invalid-provenance-key
   :invalid-provenance-reason
   :duplicate-tree-ref
   :duplicate-tree-ref-ref
   :missing-tree-entry
   :missing-tree-entry-tree-id
   :missing-tree-entry-ref
   :snapshot-format-error
   :snapshot-format-error-object-id
   :snapshot-format-error-reason
   :lzma-codec-error
   :lzma-codec-error-phase
   :lzma-codec-error-code
   :pack-error
   :pack-error-pathname
   :pack-error-reason
   :pack-integrity-error
   :pack-integrity-object-id))
