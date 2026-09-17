(uiop:define-package :tek9-tests
  (:use :tek9 :star-git :cl :fiveam)
  (:documentation "Tek9 and star-git tests."))

(in-package :tek9-tests)
(def-suite :tek9-tests
  :description "Tests for tek9 and star-git")
