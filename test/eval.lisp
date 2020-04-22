(in-package :seed/test)

(defsuite (test/eval :in test))

(in-suite test/eval)

(deftest test/eval/binding/1 ()
  (let ((bound (maru/intern "bound"))
        (unbound (maru/intern "zork"))
        (env (global-namespace-of *eval-context*)))
    (maru/define env bound 42)
    (is (eql 42 (maru/cdr (maru/find-variable env bound))))
    (is (eql nil (maru/find-variable env unbound)))
    (signals error (maru/find-variable env unbound :otherwise :error))
    (values)))
