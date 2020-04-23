(in-package :seed/eval)

;;; design notes:
;;;   - this lisp code is written with assembly in mind, i.e. to be reasonable similar to
;;;     the assembly code.
;;;
;;; concepts:
;;;   - IR (intermediate representation) is a language that can be eval'd directly in common lisp.
;;;     This greatly helps debugging. This IR can then be compiled into native assembly of your choice.
;;;

(deftype cell-type () 'fixnum)
(deftype cell-vector () '(vector cell-type))

(defstruct (workspace
             (:conc-name wp/)
             (:constructor %make-workspace))
  (data nil :type cell-vector)
  (current-index 0 :type integer))

(defun make-workspace (size)
  (%make-workspace :data (make-array size :element-type 'cell-type :initial-element 0)
                   :current-index 0))

(defstruct (runtime-env
             (:conc-name runtime-env/)
             (:constructor %make-runtime-env))
  (stack nil :type workspace)
  (words nil :type hash-table) ;; string -> instruction-list
  ;(workspaces nil :type workspace)
  ;; the integers stored in WORDS will point into WORD-BLOBS
  ;(%word-blobs nil :type cell-vector)
  )

(defun make-runtime-env ()
  (let ((env (%make-runtime-env :stack (make-workspace 20)
                                :words (make-hash-table :test 'equal))))
    #+nil ;; TODO delme
    (flet ((boot (name body)
             (define-word name body env)))
      (dolist (e '((seed::+ (wp/push (+ (wp/pop -stack-) (wp/pop -stack-)) -stack-))
                   (seed::- (wp/push (- (wp/pop -stack-) (wp/pop -stack-)) -stack-))
                   (seed::* (wp/push (* (wp/pop -stack-) (wp/pop -stack-)) -stack-))
                   (seed::/ (wp/push (round (/ (wp/pop -stack-) (wp/pop -stack-))) -stack-))))
        (apply #'boot e)))
    env))

(defstruct (compiler
             (:conc-name compiler/))
  ;; an alist of encountered definitions
  (env nil :type list)
  (word-id-counter 0 :type integer))

(defun seed/error (format &rest args)
  (apply 'error format args))

(defun seed/warn (format &rest args)
  (apply 'warn format args))

(defun seed/eval/fn (prg env)
  (cl:eval
    `(let ((-env- ,env)
           (-stack- ,(runtime-env/stack env))
           (-words- ,(runtime-env/words env)))
       (declare (ignorable -env- -stack- -words-))
       ,@prg))
  env)

(defmacro seed/eval* (&body prg)
  `(let* ((-env- (make-runtime-env))
          (-stack- (runtime-env/stack -env-))
          (-words- (runtime-env/words -env-)))
     (declare (ignorable -env- -stack- -words-))
     ,@(compile-to-ir (seed/reintern-symbols prg) (make-compiler))
     (values (wp/pop -stack-) -env-)))

(defun seed/reintern-symbols (prg)
  (labels
      ((recurse (e)
         (typecase e
           (null nil)
           (cons
            (cons (recurse (car e))
                  (recurse (cdr e))))
           (symbol
            (intern (symbol-name e) (find-package :seed)))
           (t e))))
    (recurse prg)))

(defun seed/read (input)
  (etypecase input
    (string (with-input-from-string (stream input)
              (seed/read stream)))
    (stream (with-standard-io-syntax
              (let ((*package* (find-package :seed))
                    (*read-eval* nil))
                (loop :with form
                      :while (not (eq 'eof (setf form (cl:read input nil 'eof))))
                      :collect form))))))

(defun seed/read-and-eval (input)
  (let* ((prg (seed/read input))
         (c (make-compiler))
         (ir (compile-to-ir prg c))
         (runtime (make-runtime-env)))
    (seed/eval/fn ir runtime)
    (wp/pop (runtime-env/stack runtime))))

(defun wp/push (val w)
  (setf (aref (wp/data w) (wp/current-index w)) val)
  (incf (wp/current-index w))
  val)

(defun wp/pop (w)
  (decf (wp/current-index w))
  (let ((val (aref (wp/data w) (wp/current-index w))))
    (setf (aref (wp/data w) (wp/current-index w)) 0) ; TODO
    val))

(defmacro seed-ir::push (arg)
  `(wp/push ,arg -stack-))

#+nil
(defmacro seed-ir::drop ()
  `(progn
     (wp/pop -stack-)
     (values)))

(defun define-word (name body env)
  (check-type name (and symbol (not null)))
  (setf (gethash name (runtime-env/words env))
        body))

(defmacro seed-ir::define (word &body body)
  `(setf (gethash ',word -words-)
         ',body))

(defun %ir-call (word -stack- -words- -env-)
  (case word
    ;; special forms
    (seed-ir::+ (wp/push (+ (wp/pop -stack-) (wp/pop -stack-)) -stack-))
    (seed-ir::- (wp/push (- (wp/pop -stack-) (wp/pop -stack-)) -stack-))
    (seed-ir::* (wp/push (* (wp/pop -stack-) (wp/pop -stack-)) -stack-))
    (seed-ir::/ (wp/push (round (/ (wp/pop -stack-) (wp/pop -stack-))) -stack-))
    (otherwise
     (let ((prg (gethash word -words-)))
       (seed/eval/fn prg -env-)))))

(defmacro seed-ir::call (word)
  `(%ir-call ',word -stack- -words- -env-))

(defun compiler/allocate-word-id (c)
  (prog1
      (compiler/word-id-counter c)
    (incf (compiler/word-id-counter c))))

(defun compiler/extend-env (c name body static? &key form)
  (check-type name (and symbol (not null)))
  (let ((env-entry (assoc name (compiler/env c))))
    (when env-entry
      (seed/warn "redefinition of '~S, form ~S" name form))
    (when (and env-entry
               (not (eq (getf env-entry :static)
                        static?)))
      (seed/error "changing staticness in redefinition of '~S, form ~S" name form))
    (if env-entry
        (progn
          (setf (second env-entry) body)
          (setf (getf (cddr env-entry) :static) static?))
        (setf env-entry (setf (compiler/env c) (cons (list name body :static static?)
                                                     (compiler/env c)))))
    env-entry))

(defun compiler/lookup-env (c name)
  (check-type name (and symbol (not null)))
  (assoc name (compiler/env c)))

(defun compile-to-ir (prg c)
  (let ((instructions (list)))
    (labels
        ((emit (i)
           (push i instructions))
         (recurse (form)
           (etypecase form
             (null)
             (cons
              (let* ((op (first form))
                     (args (rest form)))
                (case op
                  ;; some special forms
                  ((seed::define seed::define-static)
                   (let* ((name (first args))
                          (body (rest args))
                          (static? (eq op 'seed::define-static)))
                     (compiler/extend-env c name body static? :form form)
                     (let ((compiled-body (compile-to-ir body c)))
                       (setf (car body) (car compiled-body))
                       (setf (cdr body) (cdr compiled-body)))
                     (unless static?
                       (emit `(seed-ir::define ,name ,@(compile-to-ir body c))))))
                  (otherwise (dolist (arg args)
                               (recurse arg))
                             (recurse op)))))
             (cell-type
              (emit `(seed-ir::push ,form)))
             (symbol
              (emit
               (case form
                 (seed::+ '(seed-ir::call seed-ir::+))
                 (seed::- '(seed-ir::call seed-ir::-))
                 (seed::* '(seed-ir::call seed-ir::*))
                 (seed::/ '(seed-ir::call seed-ir::/))
                 (otherwise `(seed-ir::call ,form))))))))
      (mapcar #'recurse prg))
    (reverse instructions)))
