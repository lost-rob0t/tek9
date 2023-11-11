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
   :MAKE-KEY-ID
   :DOC-VALUE
   :DEFINE-REDUCE
   :DB-PATH
   :DOC-CHANGED
   :APPLY-VIEW
   :DB-COUNT
   :PUT*
   :NEW-DOCUMENT
   :DEFINE-MAP
   :NEW-VIEW
   :PUT-JSON
   :DB-CHANGED
   :FETCH
   :PUT-BULK*
   :VIEW-MAP
   :DB-ENV
   :PUT
   :$
   :PUT-BULK
   :WITH-DATABASE
   :TOUCH-DOCUMENT
   :INSERT-RESULTS
   :NEW-DATABASE
   :DB-VIEWS
   :%*
   :OPEN-DATABASE
   :CLOSE-DATABASE
   :APPLY-VIEW-TO-DATABASE
   :ADD-VIEW
   :DB-NAME
   :CREATE-VIEW-DB
   :DOC-ID
   :WITH-VIEWS
   :VIEW-NAME
   :DELETE-VIEW))

