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
  (%make-runtime-env :stack (make-workspace 20)
                     :words (make-hash-table :test 'equal)))

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
                      :while (not (eq 'eof (setf form (read input nil 'eof))))
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

(defmacro i/push (arg)
  `(wp/push ,arg -stack-))

(defmacro i/pop ()
  `(wp/pop -stack-))

(defmacro i/+ ()
  `(wp/push (+ (wp/pop -stack-) (wp/pop -stack-)) -stack-))

(defmacro i/* ()
  `(wp/push (* (wp/pop -stack-) (wp/pop -stack-)) -stack-))

(defmacro i/define (word-id &body body)
  `(setf (gethash ,word -words-)
         ',body))

(defmacro i/call (word)
  (with-unique-names (prg)
    `(let ((,prg (gethash ,word -words-)))
       (assert ,prg)
       (seed/eval/fn ,prg -env-))))

(defun mangle-word-name (x)
  (check-type x symbol)
  (string-downcase (symbol-name x)))

(defun compiler/allocate-word-id (c)
  (prog1
      (compiler/word-id-counter c)
    (incf (compiler/word-id-counter c))))

(defun compiler/extend-env (c name body static? &key form)
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
        (setf env-entry (setf (compiler/env c) (cons (list name body :static static? :id (compiler/allocate-word-id c))
                                                     (compiler/env c)))))
    env-entry))

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
                          (static? (eq op 'define-static)))
                     (compiler/extend-env c name body static? :form form)
                     (unless static?
                       (emit `(i/define ,(mangle-word-name name) ,@(compile-to-ir body c))))))
                  (otherwise (dolist (arg args)
                               (recurse arg))
                             (recurse op)))))
             (cell-type
              (emit `(i/push ,form)))
             (symbol
              (case form
                (seed::+ (emit `(i/+)))
                (seed::* (emit `(i/*)))
                (otherwise (emit `(i/call ,(mangle-word-name form)))))))))
      (mapcar #'recurse prg))
    (reverse instructions)))
