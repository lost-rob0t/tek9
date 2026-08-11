(in-package :cl-user)

(uiop:define-package :tek9
  (:use :cl)
  (:import-from :serapeum
                :dict
                :dict*
                :@
                :href
                :pophash)
  (:export
   ;; database
   :database :new-database :open-database :close-database :db-is-open-p
   :db-path :db-env :db-name :db-max-size :db-max-dbs :db-max-readers
   :db-durability :db-count :db-changed :db-views :database-db
   :database-stats :map-database :with-database :clear-changes
   ;; documents
   :document :new-document :doc-id :doc-value :doc-changed :touch-document
   :untouch-document :make-key-id :put :put* :put-bulk :put-bulk* :put-json
   :fetch :fetch* :fetch-bulk :fetch-bulk* :delete-document
   ;; indexes and query
   :secondary-index :register-index :unregister-index :rebuild-index
   :rebuild-index-fast :bulk-load :clear-index :secondary-index-by-name
   :index-name :index-source-db :index-key-type :index-unique-p
   :index-document-ids :index-fetch :select :select-primary-range
   :select-index :select-index-range
   ;; graph
   :node :node-id :node-props :node-edges :edge :edge-id :edge-source
   :edge-predicate :edge-target :edge-key :get-default-graph-db :get-graph-db
   :put-node :put-nodes :fetch-node :fetch-bulk-nodes :add-node-edge :put-edge
   :put-edges :put-edges* :fetch-node-edge-ids :fetch-node-edges
   :fetch-node-neighbors :delete-edge
   ;; views
   :database-view :new-view :add-view :delete-view :clear-view :create-view-db
   :define-map :define-reduce :insert-results :apply-view
   :apply-view-to-database :with-views
   ;; encoding
   :%* :$))
