(in-package :cl-user)

(defpackage :seed
  (:use :common-lisp
        :alexandria
        :anaphora
        :iterate)
  (:shadow
   ;; shadow some stuff to avoid confusion
   #:eval
   )
  (:documentation "Package for the eval implementation."))
