(in-package :cl-user)

(defpackage :seed/test
  (:use :seed
        :common-lisp
        :hu.dwim.util
        :hu.dwim.logger
        :hu.dwim.stefil)
  (:shadow
   #:deftest
   #:eval))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; from hu.dwim.common
  (defun import-all-owned-symbols (source-package target-package &key (overwrite nil))
    (declare (optimize (debug 3)))
    (setf source-package (find-package source-package))
    (setf target-package (find-package target-package))
    (let ((count 0))
      (do-symbols (symbol source-package)
        (let ((target-symbol-with-same-name (find-symbol (symbol-name symbol) target-package)))
          (when (and (eq (symbol-package symbol) source-package)
                     (or overwrite
                         (not target-symbol-with-same-name)))
            (when (and target-symbol-with-same-name
                       (not (eq symbol target-symbol-with-same-name))
                       overwrite)
              (unintern target-symbol-with-same-name target-package))
            (shadowing-import symbol target-package)
            (incf count))))
      count)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; from hu.dwim.common
  (import-all-owned-symbols :seed/ia32 :seed/test :overwrite t)
  (import-all-owned-symbols :seed/eval :seed/test :overwrite t))
