(ns sicp.test.chapter-2.universal-arithmetic
  (:use sicp.chapter-2.universal-arithmetic)
  (:use clojure.test))

(deftest can-add-integers
  (is (= (add 2 3) 5)))

(deftest can-subtract-integers
  (is (= (sub 2 3) -1)))

(deftest can-multiply-integers
  (is (= (mul 2 3) 6)))

(deftest can-divide-integers
  (is (= (div 6 3) 2)))
