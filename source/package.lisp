(in-package :cl-user)

(defpackage :seed/eval
  (:use :common-lisp
        :alexandria
        :anaphora
        :iterate)
  (:shadow
   ;; shadow some stuff to avoid confusion
   #:eval
   )
  (:documentation "Package for the eval implementation."))

(defpackage :seed
  (:use)
  (:documentation "Empty package that will be used when READ'ing seed files."))
