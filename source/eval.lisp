(in-package :seed)

(deftype cell-type () 'fixnum)
(deftype cell-vector () '(vector cell-type))

(defstruct (workspace
             (:conc-name wp/)
             (:predicate nil)
             (:copier nil)
             (:constructor %make-workspace))
  (data nil :type cell-vector)
  (current-index 0 :type integer))

(defstruct (env
             (:conc-name env/)
             (:predicate nil)
             (:copier nil)
             (:constructor %make-env))
  (stack nil :type workspace)
  (words nil :type hash-table) ;; string -> instruction-list
  ;(workspaces nil :type workspace)
  ;; the integers stored in WORDS will point into WORD-BLOBS
  ;(%word-blobs nil :type cell-vector)
  )

(defun make-workspace (size &key (initial-contents nil initial-contents?))
  (%make-workspace :data (apply 'make-array size
                                :element-type 'cell-type
                                (if initial-contents?
                                    `(:initial-contents ,initial-contents)
                                    '(:initial-element 0)))
                   :current-index (if initial-contents?
                                      (length initial-contents)
                                      0)))

(defun make-env ()
  (%make-env :stack (make-workspace 20)
             :words (make-hash-table :test 'equal)))

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

(defun seed/compile (prg)
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
                            (emit `(i/define ,(mangle-word-name name) ,@(seed/compile body)))))
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
          (-stack- ,(env/stack env))
          (-words- ,(env/words env)))
      (declare (ignorable -env- -stack- -words-))
      ,@prg))
  env)

(defmacro seed/eval* (&body prg)
  `(let* ((-env- (make-env))
          (-stack- (env/stack -env-))
          (-words- (env/words -env-)))
     (declare (ignorable -env- -stack- -words-))
     ,@(seed/compile prg)
     (values (wp/pop -stack-) -env-)))
