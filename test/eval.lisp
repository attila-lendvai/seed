(in-package :seed/test)

(defsuite (test/eval :in test))

(in-suite test/eval)

(deftest test/eval/1 ()
  (is (= 42
         (eval
          (define bar
            (+ 2))
          (define foo
            (bar (+ 20)))
          (foo (+ (* 2 5) 10))))))
