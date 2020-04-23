(in-package :seed/test)

(defsuite (test/eval :in test))

(in-suite test/eval)

(deftest test/eval/1 ()
  (is (eql 42 (read-and-eval
                "(define bar
                   (+ 2))
                 (define foo
                   (bar (+ 20)))
                 (foo (+ (* 2 5) 10))"))))

(deftest test/eval/2 ()
  (is (eql 42 (read-and-eval
                "(define bar
                   (foo 2))
                 (define foo
                   (+ 20))
                 (+ (bar) 20)"))))
