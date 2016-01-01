(ns sicp.test.chapter-4.evaluator
  (:use sicp.chapter-4.evaluator)
  (:use clojure.test))

(deftest can-evaluate-numbers
  (is (= (my-eval '4 {}) 4)))

(deftest can-evaluate-strings
  (is (= (my-eval '"steve" {}) "steve")))

(deftest can-quote
  (is (= (my-eval '(quote (+ 2 3)) {}) '(+ 2 3))))
