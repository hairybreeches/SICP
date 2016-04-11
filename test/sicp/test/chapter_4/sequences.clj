(ns sicp.test.chapter-4.sequences
  (:use sicp.test.chapter-4.evaluator)
  (:use sicp.chapter-4.sequences)
  (:use clojure.test))

(deftest can-exclude
  (evals-to '(1 3 5 7 9)
            filter-code
            member?
            exclude-code
            '(exclude '(2 4 6 8) '(1 2 3 4 5 6 7 8 9))))

(deftest can-map
  (evals-to '(2 4 6 8 10)
            map-code
            '(map (lambda (x) (* 2 x)) '(1 2 3 4 5))))
