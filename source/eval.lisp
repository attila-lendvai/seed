(in-package :seed)

;;; design notes:
;;;   - this lisp code is written with assembly in mind, i.e. to be reasonable similar to
;;;     the assembly code.
;;;
;;; concepts:
;;;   - IR (intermediate representation) is a language that can be eval'd directly in common lisp.
;;;     This greatly helps debugging. This IR can then be compiled into native assembly of your choice.
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *debug-only* t))

(defmacro debug-only (&body body)
  (when *debug-only*
    `(progn ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *safe-only* t))

(defmacro safe-only (&body body)
  (when *safe-only*
    `(progn ,@body)))

(defun seed/error (format &rest args)
  (apply 'error format args))

(defun seed/warn (format &rest args)
  (apply 'warn format args))

;;;
;;;
;;;

(deftype cell-value        () 'fixnum)
(deftype cell-vector       () '(vector cell-value))
(deftype cell-vector-index () 'fixnum)

(deftype word-name () 'string)

(defun random-cell-calue ()
  (let ((num (random (* most-positive-fixnum 2))))
    (when (>= num most-positive-fixnum)
      (setf num (- (mod num 2))))
    (the cell-value num)))

(defstruct (eval-env
             (:conc-name eval-env/)
             (:constructor %make-eval-env))
  "The runtime environment when the IR is evaluated in Common Lisp."
  ;; MEMORY-BLOCK is the single block of memory that is available for user code.
  ;; the API to access it is through two stacks at both ends of this memory block,
  ;; growing towards each other. they are called 'workspace' and 'stack'.
  ;; when compiled to the hw, 'stack' is normally implemented using the CPU's call stack.
  (memory-block nil :type cell-vector)
  (stack-index nil :type cell-vector-index)
  (workspace-index nil :type cell-vector-index))

(defun make-eval-env (&key (memory-size 100))
  (let ((memory-block (make-array memory-size :element-type 'cell-value :initial-element 0)))
    (%make-eval-env :memory-block memory-block
                    ;; with both stacks we point one index outside
                    :stack-index (length memory-block)
                    :workspace-index -1)))

;;;
;;; stack operations
;;;

(defun stack-collision (env)
  (declare (ignore env))
  (seed/error "runtime error: the workspace and the stack has collided"))

(defun push/stack (val env)
  (check-type env eval-env)
  (safe-only (when (<= (1- (eval-env/stack-index env))
                       (eval-env/workspace-index env))
               (stack-collision env)))
  (setf (aref (eval-env/memory-block env)
              (decf (eval-env/stack-index env)))
        val))

(defun pop/stack (env)
  (check-type env eval-env)
  (let* ((index (eval-env/stack-index env))
         (val (aref (eval-env/memory-block env)
                    index)))
    (debug-only (setf (aref (eval-env/memory-block env) index) (random-cell-calue)))
    (incf (eval-env/stack-index env))
    val))

(defun push/wp (val env)
  (check-type env eval-env)
  (safe-only (when (>= (1+ (eval-env/workspace-index env))
                       (eval-env/stack-index env))
               (stack-collision env)))
  (setf (aref (eval-env/memory-block env)
              (incf (eval-env/workspace-index env)))
        val))

(defun pop/wp (env)
  (check-type env eval-env)
  (let* ((index (eval-env/workspace-index env))
         (val (aref (eval-env/memory-block env)
                    index)))
    (debug-only (setf (aref (eval-env/memory-block env) index) (random-cell-calue)))
    (decf (eval-env/workspace-index env))
    val))

;;;
;;; compile to IR
;;;

(defstruct (compiler
             (:conc-name compiler/)
             (:constructor %make-compiler))
  (compile-time-eval-env nil :type eval-env))

(defun make-compiler (&key (memory-size 100))
  (let ((eval-env (make-eval-env :memory-size memory-size)))
    (%make-compiler :compile-time-eval-env eval-env)))

(defstruct (compiler/env-entry
             (:conc-name compiler/env/))
  (name nil) ; this holds the flat, nested-name string of non-primitive definitions
  (source-form nil)
  (input-arguments nil)
  (output-arguments nil)
  (ir-primitive nil)
  (extern nil))

(defvar *compile/verbose* nil)

(defparameter *seed-bootstrap-definitions*
  (read-seed
   "
 (define + (2 1)           (primitive SEED/IR:+))
 (define * (2 1)           (primitive SEED/IR:*))
"))

(defvar *ir-definitions*)

(defun %compile/seed-to-ir (prg c name-prefix env)
  (check-type c compiler)
  (let ((instructions (list)))
    (labels
        ((emit (i)
           (push i instructions))
         (extend-env (env-name entry)
           (check-type env-name (and symbol (not null)))
           (when (lookup-env env-name)
             (seed/warn "redefinition/shadowing of '~S~:[~;, by form ~S~]"
                        (compiler/env/name entry)
                        (compiler/env/source-form entry)
                        (compiler/env/source-form entry)))
           (setf env (cons (cons env-name entry) env))
           (values))
         (lookup-env (env-name)
           (check-type env-name (and symbol (not null)))
           (cdr (assoc env-name env :test 'equal)))
         (nested-name (name)
           (if name-prefix
               (concatenate 'string name-prefix "/" (symbol-name name))
               (symbol-name name)))
         (compile-define (form)
           (let ((ir-primitive nil)
                 (extern-name nil))
             (destructuring-bind (name arg-spec &rest body) (rest form)
               (assert (and name (symbolp name)))
               (destructuring-bind (&optional input-args output-args) arg-spec
                 (when (consp input-args)
                   (setf input-args (length input-args)))
                 (when (consp output-args)
                   (setf output-args (length output-args)))
                 (when (and (consp body)
                            (consp (car body)))
                   (case (first (first body))
                     (seed/src::|primitive|
                      (when (or (rest body)
                                (> (length (first body)) 2))
                        (seed/error "invalid primitive declaration ~S" form))
                      (setf ir-primitive (second (first body))))
                     (seed/src::|extern|
                      (setf extern-name (symbol-name (second (first body)))))))
                 (let ((nested-name (nested-name name)))
                   (extend-env name (make-compiler/env-entry :name nested-name
                                                             :input-arguments input-args
                                                             :output-arguments output-args
                                                             :source-form form
                                                             :ir-primitive ir-primitive
                                                             :extern extern-name))
                   (unless (or ir-primitive
                               extern-name)
                     (push `(seed/ir:define ,nested-name (,input-args ,output-args)
                                            ,@(%compile/seed-to-ir body c nested-name env))
                           *ir-definitions*)))))))
         (recurse (form)
           (etypecase form
             (cons
              ;;
              ;; a function call
              ;;
              (let* ((op (first form))
                     (args (rest form)))
                (case op
                  ;; some special forms
                  (seed/src::|define|
                   (compile-define form))
                  (otherwise            ; it's a simple call
                   (dolist (arg args)
                     (recurse arg))
                   (let* ((target-definition (or (lookup-env op)
                                                 (seed/error "unknown word ~S, in form ~S" op form)))
                          (primitive (compiler/env/ir-primitive target-definition))
                          (target-name (or primitive
                                           (compiler/env/name target-definition))))
                     (aif (compiler/env/extern target-definition)
                          (emit `(seed/ir:call (seed/ir:extern ,it)))
                          (progn
                            (assert target-name)
                            (emit `(seed/ir:call ,target-name)))))))))
             (cell-value
              (emit `(seed/ir:push/stack ,form))))))
      (map nil #'recurse prg))
    (nreverse instructions)))

(defun compile/seed-to-ir (prg c &key verbose)
  (let* ((*compile/verbose* verbose)
         (*ir-definitions* ())
         (ir (%compile/seed-to-ir (append *seed-bootstrap-definitions*
                                          prg)
                                  c nil nil)))
    (when verbose
      (format *debug-io* "IR definitions: ~S~%IR: ~S~%" *ir-definitions* ir))
    (append (nreverse *ir-definitions*)
            ir)))

;;;
;;; compile to lisp and eval in lisp
;;;

(defmacro seed/eval* (&body seed-prg)
  `(let* ((-env- (make-eval-env))
          (-memory-block- (eval-env/memory-block -env-)))
     (declare (ignorable -env- -memory-block-))
     ,@(compile/ir-to-lisp (compile/seed-to-ir seed-prg (make-compiler)))
     (values (pop/stack -env-) -env-)))

(defun seed/eval-ir (ir-prg env &key verbose)
  (check-type env eval-env)
  (let ((lisp-forms (compile/ir-to-lisp ir-prg)))
    (when verbose
      (format *debug-io* "Lisp: ~S~%" lisp-forms))
    (cl:eval
      `(let ((-env- ,env)
             (-memory-block- ,(eval-env/memory-block env)))
         (declare (ignorable -env- -memory-block-))
         ,@lisp-forms)))
  env)

(defun read-seed (input)
  (etypecase input
    (string (with-input-from-string (stream input)
              (read-seed stream)))
    (stream (with-standard-io-syntax
              (let ((*package* (find-package :seed/src))
                    (*read-eval* nil)
                    (*readtable* (let ((table (with-standard-io-syntax
                                                (copy-readtable *readtable*))))
                                   (setf (readtable-case table) :preserve)
                                   table)))
                (loop :with form
                      :while (not (eq 'eof (setf form (cl:read input nil 'eof))))
                      :collect form))))))

(defun read-seed-and-eval (input &key verbose)
  (let* ((prg (read-seed input))
         (c (make-compiler))
         (ir (compile/seed-to-ir prg c :verbose verbose))
         (eval-env (make-eval-env)))
    (seed/eval-ir ir eval-env :verbose verbose)
    (values (pop/stack eval-env) eval-env)))

(defun compile/ir-to-lisp (prg)
  (let* ((defs ())
         (tlfs (remove nil (mapcar (lambda (def)
                                     (if (and (consp def)
                                              (eq (car def) 'seed/ir::define))
                                         (progn
                                           (push def defs)
                                           nil)
                                         def))
                                   prg)))
         (labels-entries (mapcar (lambda (def)
                                   (destructuring-bind (name arg-spec &rest def-prg) (rest def)
                                     (declare (ignore arg-spec))
                                     `(,(intern name :seed/src) () ,@(compile/ir-to-lisp def-prg))))
                                 defs)))
    (if labels-entries
        `((labels (,@labels-entries)
            ,@tlfs))
        `(,@tlfs))))

(defmacro seed/ir:push/wp (arg)
  `(push/wp ,arg -env-))

(defmacro seed/ir:push/stack (arg)
  `(push/stack ,arg -env-))

(defmacro seed/ir:call (word)
  (case word
    ;; special forms
    (seed/ir:+ '(push/stack (+ (pop/stack -env-) (pop/stack -env-)) -env-))
    (seed/ir:- '(push/stack (- (pop/stack -env-) (pop/stack -env-)) -env-))
    (seed/ir:* '(push/stack (* (pop/stack -env-) (pop/stack -env-)) -env-))
    (seed/ir:/ '(push/stack (round (/ (pop/stack -env-) (pop/stack -env-))) -env-))
    (otherwise `(funcall #',(intern word :seed/src)))))
