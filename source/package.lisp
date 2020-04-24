(in-package :cl-user)

(defpackage :seed
  (:use :common-lisp
        :alexandria
        :anaphora
        :iterate)
  (:shadow
   ;; shadow some stuff to avoid confusion
   #:eval
   #:compile
   #:read
   #:read-from-string
   )
  (:export
   #:seed/error
   #:seed/warn
   #:system-relative-pathname
   )
  (:documentation "Package for the eval implementation."))

(defpackage :seed/ia32
  (:use :common-lisp
        :alexandria
        :anaphora
        :iterate
        :seed))

(defpackage :seed/src
  (:use)
  (:documentation "Empty package that will be used when READ'ing seed files."))

(defpackage :seed/ir
  (:use)
  (:export
   #:define
   #:push
   #:call
   #:+
   #:-
   #:*
   #:/
   )
  (:documentation "Empty package that will be used for the SEED IR operators"))
