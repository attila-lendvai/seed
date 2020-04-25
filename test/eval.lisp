(in-package :seed/test)

(defsuite (test/eval :in test))

(in-suite test/eval)

(deftest test/eval/1 ()
  (is (eql 42 (read-and-eval
                "(define bar ()
                   (+ 2))
                 (define foo ()
                   (bar (+ 21)))
                 (foo (+ (* 2 5) 9))"))))

(deftest test/eval/2 ()
  (is (eql 42 (read-and-eval
                "(define foo ()
                   (+ 20))
                 (define bar ()
                   (foo 2))
                 (+ (bar) 20)"))))
