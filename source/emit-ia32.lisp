(in-package :seed/ia32)

;;; design notes:
;;;
;;; concepts:
;;;

(defparameter *ia32-prologue*
"
	.macro M_rot2
	movl	(%esp),	%eax
	movl	4(%esp),%ebx
	movl	%ebx,	(%esp)
	movl	%eax,	4(%esp)
	.endm

	.macro M_rot3
	movl	(%esp),	%eax
	movl	4(%esp),%ebx
	movl	%ebx,	(%esp)
	movl	8(%esp),%ebx
	movl	%ebx,	4(%esp)
	movl	%eax,	8(%esp)
	.endm
")

(defun mangle-ir-name (x)
  (etypecase x
    (cons
     (assert (eq (first x) 'seed/ir:extern))
     (second x))
    (string
     (setf x (substitute #\_ #\/ x))
     (setf x (substitute #\_ #\- x))
     (concatenate 'string "L_" x))))

(defun compile-to/ia32/static (ir &rest args &key (stream 'string) (memory-size 10) verbose &allow-other-keys)
  (declare (ignore memory-size verbose))
  (if (or (eq stream 'string)
          (null stream))
      (with-output-to-string (stream)
        (apply 'compile-to/ia32/static ir :stream stream args))
      (apply '%compile-to/ia32/static ir args)))

(defun %compile-to/ia32/static (ir &key (stream *standard-output*) (memory-size 10) verbose (safety 0))
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
           (emit-label (name)
             (emit name ":"))
           (literal (value)
             (concatenate 'string "$" (princ-to-string value)))
           (emit-push/stack (value)
             (emit "	pushl	" value))
           (emit-pop/stack (target)
             (emit "	pop	" target))
           #+nil
           (emit-push/wp (value)
             (emit "	subl	$4, %ebp")
             (emit "	movl	" value ", (%ebp)"))
           #+nil
           (emit-pop/wp (target)
             (emit "	movl	(%ebp)," target)
             (emit "	addl	$4, %ebp"))
           (emit-rot (n)
             (ecase n
               (2
                (emit "	M_rot2"))
               (3
                (emit "	M_rot3"))))
           (emit-program (prg)
             (dolist (form prg)
               (when verbose
                 (emit-comment "form: " form))
               (ecase (first form)
                 (seed/ir:push/stack
                  (emit-push/stack (literal (second form))))
                 (seed/ir:call
                  (case (second form)
                    ;; handle the special forms
                    (seed/ir:+
                     (emit-pop/stack "%eax")
                     (emit "	addl	%eax, (%esp)")
                     )
                    (seed/ir:*
                     (emit-pop/stack "%eax")
                     (emit "	mull	(%esp)")
                     (emit "	movl	%eax, (%esp)"))
                    (otherwise
                     (let ((name (second form)))
                       (emit "	call	" (mangle-ir-name name)))))))))
           (emit-definitions (defs)
             (emit-comment "begin definitions")
             (dolist (form defs)
               (emit-comment "def: " form)
               (destructuring-bind (name (in-args out-args) &rest body) (rest form)
                 (emit "	.text")
                 (emit (mangle-ir-name name) ":")
                 (unless (zerop in-args)
                   (emit-rot (1+ in-args)))
                 (emit-program body)
                 (unless (zerop out-args)
                   (emit-rot (1+ out-args)))
                 (emit "	ret")))
             (emit-comment "end definitions"))
           (emit-toplevel (tlfs)
             (unless (zerop safety)
               (emit "	.data")
               (emit-label "memory_top")
               (emit "	.long 0")
               (emit-label "memory_bottom")
               (emit "	.long 0"))
             (emit "	.text")
             (emit-comment "begin toplevel")
             (emit "	.globl main")
             (emit-label "main")
             (let ((memory-size-in-bytes (* memory-size 4)))
               ;; %esp is stack, %ebp is workspace
               (emit "	movl	%esp, %ebp")
               (emit "	subl	" (literal memory-size-in-bytes) ", %ebp")
               (unless (zerop safety)
                 (emit "	movl	%esp, memory_top")
                 (emit "	movl	%ebp, memory_bottom"))
               (emit-program tlfs))
             (emit-pop/stack "%eax")
             (emit "	ret")
             (emit-comment "end toplevel")))
        (let* ((defs ())
               (tlfs (remove nil (mapcar (lambda (def)
                                           (if (and (consp def)
                                                    (eq (car def) 'seed/ir::define))
                                               (progn
                                                 (push def defs)
                                                 nil)
                                               def))
                                         ir))))
          (setf defs (nreverse defs))
          (emit *ia32-prologue*)
          (emit-definitions defs)
          (emit-toplevel tlfs))))))
