(in-package :seed/test)

(defsuite (test/ia32 :in test))

(in-suite test/ia32)

(defun read-and-compile-to/ia32/static (src &key verbose)
  (let* ((c (make-compiler))
         (asm (compile-to/ia32/static
               (compile/seed-to-ir (read-seed src) c :verbose verbose)
               :verbose verbose)))
    (when verbose
      (write-string asm *debug-io*))
    (uiop:with-temporary-file (:pathname asm-filename :stream asm-output
                                         :prefix "seed-ia32-tmp" :type "s")
      (uiop:with-temporary-file (:pathname exe-filename)
        (with-standard-io-syntax
          (write-string asm asm-output)
          (force-output asm-output))
        (multiple-value-bind (gcc-output gcc-error-output error-code)
            (uiop:run-program `("gcc" "-m32" "-o" ,(namestring exe-filename) ,(namestring asm-filename))
                              :directory (system-relative-pathname "")
                              :output 'string
                              :error-output 'string
                              :ignore-error-status t)
          (if (eql 0 error-code)
              (uiop:run-program `(,(namestring exe-filename)))
              (format t "*** failed:~%~S~%~S~%" gcc-output gcc-error-output)))))))

(deftest test/ia32/1 ()
  (read-and-compile-to/ia32/static
   "(define bar (1 1)
      (+ 2))
    (define foo (1 1)
      (bar (+ 19)))
    (foo (+ (* 2 5) 11))"))

(deftest test/ia32/2 ()
  (read-and-compile-to/ia32/static
   "(define bar (1 1)
      (define z (2 1)
        (+))
      (+ (z 1 1)))
    (define foo (1 1)
      (bar (+ 19)))
    (foo (+ (* 2 5) 11))"))

(deftest test/ia32/extern ()
  (read-and-compile-to/ia32/static
   "(define bar (1 0) (extern printf))
    (bar (+ (* 2 5) 32))"))
