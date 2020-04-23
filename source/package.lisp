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

(defpackage :seed/ia32
  (:use :common-lisp
        :alexandria
        :anaphora
        :iterate
        :seed/eval))

(defpackage :seed
  (:use)
  (:documentation "Empty package that will be used when READ'ing seed files."))

(defpackage :seed-ir
  (:use)
  (:documentation "Empty package that will be used for the SEED IR operators"))
