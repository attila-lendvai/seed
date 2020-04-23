(in-package :seed/ia32)

;;; design notes:
;;;
;;; concepts:
;;;

(defun mangle-word-name-for-ia32 (x)
  (assert (and (symbolp x) (member (package-name (symbol-package x)) '("SEED" "SEED-IR") :test 'equal)))
  (concatenate 'string "L_" (string-downcase (symbol-name x))))

(defun compile-to-ia32 (prg c &key output)
  (if output
      (compile-to-ia32/body prg c output)
      (with-output-to-string (stream)
        (compile-to-ia32 prg c :output stream))))

(defun compile-to-ia32/body (prg c stream &key (stack-size 10))
  (with-standard-io-syntax
    (let ()
      (labels
          ((emit (&rest insts)
             (dolist (i insts)
               (princ i stream))
             (terpri stream))
           (emit-comment (&rest comment)
             (write-string "## " stream)
             (apply #'emit comment))
           (emit-push (value)
             (emit "	subl	$4, %ebp")
             (emit "	movl	" value ", (%ebp)"))
           (emit-push/literal (value)
             (emit-push (concatenate 'string "$" (princ-to-string value))))
           (emit-pop (target)
             (emit "	movl	(%ebp)," target)
             (emit "	addl	$4, %ebp"))
           #+nil
           (emit-rot ()
             (emit "	movl	(%esp), %eax")
             (emit "	movl	4(%esp), %ebx")
             (emit "	movl	%ebx, (%esp)")
             (emit "	movl	8(%esp), %ebx")
             (emit "	movl	%ebx, 4(%esp)")
             (emit "	movl	%eax, 8(%esp)"))
           #+nil
           (emit-swap ()
             (emit "	movl	(%esp), %eax")
             (emit "	movl	4(%esp), %ebx")
             (emit "	movl	%ebx, (%esp)")
             (emit "	movl	%eax, 4(%esp)"))
           (emit-program (prg)
             (dolist (form prg)
               (ecase (first form)
                 (seed-ir::push
                  (emit-push/literal (second form))
                  ;;(emit "	push	$" (second form))
                  )
                 #+nil
                 (seed-ir::drop
                  (emit "	pop"))
                 (seed-ir::call
                  (case (second form)
                    ;; handle the special forms
                    (seed-ir::+
                     (emit-pop "%eax")
                     ;;(emit "	pop	%eax")
                     (emit "	addl	%eax, (%ebp)")
                     )
                    (seed-ir::*
                     (emit-pop "%eax")
                     ;;(emit "	pop	%eax")
                     (emit "	mull	(%ebp)")
                     (emit "	movl	%eax, (%ebp)"))
                    (otherwise
                     (let* ((name (second form))
                            (env-entry (seed/eval::compiler/lookup-env c name)))
                       (if env-entry
                           (emit "	call	" (mangle-word-name-for-ia32 name))
                           (seed/eval::seed/error "unknown definition called in IR form ~S" form))))))
                 ;;(seed-ir::define (not-yet-implemented))
                 )))
           (emit-definitions ()
             (emit-comment "begin definitions")
             (dolist (env-entry (reverse (seed/eval::compiler/env c)))
               (emit-comment "def: " (princ-to-string env-entry))
               (destructuring-bind (name body &key &allow-other-keys) env-entry
                 (emit "	.text")
                 (emit (mangle-word-name-for-ia32 name) ":")
                 ;;(emit-rot)
                 (emit-program body)
                 ;;(emit-swap)
                 (emit "	ret")))
             (emit-comment "end definitions"))
           (emit-toplevel ()
             (emit-comment "begin toplevel")
             (emit "	.globl main")
             (emit "	.text")
             (emit "main:")
             (let ((stack-size-in-bytes (* stack-size 4)))
               (emit "	movl	%esp, %ebp")
               (emit "	subl	$" stack-size-in-bytes ", %esp")
               (emit-program prg)
               (emit "	addl	$" stack-size-in-bytes ", %esp"))
             (emit-pop "%eax")
             (emit "	ret")
             (emit-comment "end toplevel")))
        (emit-definitions)
        (emit-toplevel)))))
