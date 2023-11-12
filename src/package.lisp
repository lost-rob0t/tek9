(in-package :cl-user)
(uiop:define-package :tek9
  (:use :cl)
  (:import-from :serapeum
   :dict :href
   :dict* :@
   :pophash)
  (:export
   :FETCH*
   :VIEW-REDUCE
   :DOC-VALUE
   :FETCH-BULK
   :NEW-DOCUMENT
   :DB-PATH
   :OPEN-DATABASE
   :MAKE-KEY-ID
   :DB-ENV
   :FETCH
   :APPLY-VIEW
   :DB-COUNT
   :DB-VIEWS
   :NEW-DATABASE
   :PUT*
   :DEFINE-MAP
   :PUT-BULK*
   :PUT-JSON
   :DB-CHANGED
   :VIEW-MAP
   :DEFINE-REDUCE
   :$
   :PUT-BULK
   :DOC-CHANGED
   :TOUCH-DOCUMENT
   :DB-MAX-SIZE
   :PUT
   :FETCH-BULK*
   :CLOSE-DATABASE
   :APPLY-VIEW-TO-DATABASE
   :CREATE-VIEW-DB
   :DB-NAME
   :WITH-VIEWS
   :VIEW-NAME
   :DELETE-VIEW
   :%*
   :ADD-VIEW
   :INSERT-RESULTS
   :DOC-ID
   :NEW-VIEW))

   
