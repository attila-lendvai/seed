(in-package :seed/test)

(defsuite (test/ia32 :in test))

(in-suite test/ia32)

(deftest read-and-compile-to-ia32-asm (form)
  (let* ((c (seed/eval::make-compiler))
         (asm (compile-to-ia32
               (compile-to-ir
                (seed/read form)
                c)
               c)))
    (print asm)
    (uiop:with-temporary-file (:pathname asm-filename :stream asm-output
                               :prefix "seed-ia32-tmp" :type "s")
      (uiop:with-temporary-file (:pathname exe-filename)
        (with-standard-io-syntax
          (write-string asm asm-output)
          (force-output asm-output))
       (multiple-value-bind (gcc-output gcc-error-output error-code)
           (uiop:run-program `("gcc" "-m32" "-o" ,(namestring exe-filename) ,(namestring asm-filename))
                             :directory (seed/eval::system-relative-pathname "")
                             :output 'string
                             :error-output 'string
                             :ignore-error-status t)
         (if (eql 0 error-code)
             (uiop:run-program `(,(namestring exe-filename)))
             (format t "*** failed:~%~S~%~S~%" gcc-output gcc-error-output)))))))

(deftest test/ia32/1 ()
  (read-and-compile-to-ia32-asm
   "(define-static bar
      (+ 2))
    (define-static foo
      (bar (+ 19)))
    (foo (+ (* 2 5) 11))"))
