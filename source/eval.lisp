(in-package :seed/eval)

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
  (definitions nil :type list))

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

(defmacro i/define (word &body body)
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

(defun seed/compile-to-ir (prg)
  (let ((stack (list)))
    (labels
        ((emit (i)
           (push i stack))
         (recurse (e)
           (etypecase e
             (null)
             (cons
              (let* ((op (first e))
                     (args (rest e)))
                (case op
                  ;; some special forms
                  (define (let ((name (first args))
                                (body (rest args)))
                            (emit `(i/define ,(mangle-word-name name) ,@(seed/compile-to-ir body)))))
                  (otherwise (dolist (arg args)
                               (recurse arg))
                             (recurse op)))))
             (cell-type
              (emit `(i/push ,e)))
             (symbol
              (case e
                (+ (emit `(i/+)))
                (* (emit `(i/*)))
                (otherwise (emit `(i/call ,(mangle-word-name e)))))))))
      (mapcar #'recurse prg))
    (reverse stack)))

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
     ,@(seed/compile-to-ir prg)
     (values (wp/pop -stack-) -env-)))
