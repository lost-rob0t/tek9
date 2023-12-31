(in-package :cl-user)
(uiop:define-package :tek9
  (:use :cl)
  (:import-from :serapeum
   :dict :href
   :dict* :@
   :pophash)
  (:export :NODE-PROPS
   :%*
           :TOUCH-DOCUMENT
   :DOC-ID
           :NODE-EDGES
   :WITH-VIEWS
           :DB-CHANGED
   :NODE-ID
           :NEW-VIEW
   :MAKE-KEY-ID
           :ADD-NODE-EDGE
   :GET-GRAPH-DB
           :DEFINE-REDUCE
   :EDGE-SOURCE
           :FETCH-BULK*
   :UNTOUCH-DOCUMENT
           :APPLY-VIEW
   :WITH-DATABASE
           :FETCH-BULK
   :CLOSE-DATABASE
           :PUT-EDGE
   :VIEW-REDUCE
           :FETCH
   :PUT
           :DOC-CHANGED
   :FETCH*
           :PUT*
   :PUT-JSON
           :DOC-VALUE
   :PUT-EDGES*
           :FETCH-BULK-NODES
   :PUT-BULK
           :CREATE-VIEW-DB
   :OPEN-DATABASE
           :DEFINE-MAP
   :EDGE-PREDICATE
           :DB-PATH
   :INSERT-RESULTS
           :PUT-EDGES
   :VIEW-NAME
           :DB-ENV
   :ADD-VIEW
           :EDGE-TARGET
   :GET-DEFAULT-GRAPH-DB
           :NEW-DOCUMENT
   :DB-NAME
           :DB-MAX-SIZE
   :DELETE-VIEW
           :DB-VIEWS
   :NEW-DATABASE
           :PUT-BULK*
   :FETCH-NODE-NEIGHBORS
           :$
   :VIEW-MAP
           :DB-COUNT
   :FETCH-NODE
           :select
   :APPLY-VIEW-TO-DATABASE
           :database
   :db-is-open-p))
