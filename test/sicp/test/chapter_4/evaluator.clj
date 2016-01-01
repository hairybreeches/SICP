(ns sicp.test.chapter-4.evaluator
  (:use sicp.chapter-4.evaluator)
  (:use clojure.test))

(deftest can-evaluate-numbers
  (is (= (my-eval '4 {}) 4)))

(deftest can-evaluate-strings
  (is (= (my-eval '"steve" {}) "steve")))

(deftest can-quote
  (is (= (my-eval '(quote (+ 2 3)) {}) '(+ 2 3))))

(deftest can-branch-true
  (is (= (my-eval '(if true 4 3) {}) 4)))

(deftest numbers-are-true
  (is (= (my-eval '(if 2 4 3) {}) 4)))

(deftest can-branch-false
  (is (= (my-eval '(if false 4 3) {}) 3)))

(deftest can-branch-false-with-no-alternative
  (is (= (my-eval '(if false 4) {}) false)))

(deftest can-cond-to-first
  (is (= (my-eval
           '(cond
              (true 1)
              (false 4 3))
                  {}) 1)))

(deftest can-cond-to-second
  (is (= (my-eval
           '(cond
              (false 1)
              (3 4)
              (4 2))
                  {}) 4)))

(deftest can-cond-to-last
  (is (= (my-eval
           '(cond
              (false 1)
              (false 4)
              (else 8))
                  {}) 8)))
