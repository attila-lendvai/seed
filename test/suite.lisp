(in-package :seed/test)

(defsuite (test :in root-suite))

(defmacro deftest (name args &body body)
  `(hu.dwim.stefil:deftest ,name ,args
     (let ((*print-circle* t)
           ;; delme? (*default-pathname-defaults* (asdf:system-relative-pathname :maru ""))
           )
       (progn ;with-new-maru-state
         ,@body))))

(defmacro eval (&body body)
  `(seed/eval* ,@body))
