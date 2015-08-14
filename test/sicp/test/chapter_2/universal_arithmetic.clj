(ns sicp.test.chapter-2.universal-arithmetic
  (:use sicp.chapter-2.rational-numbers)
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

(deftest can-add-rationals
  (is (= (add (make-rat 1 2) (make-rat 2 3)) (make-rat 7 6))))

(deftest can-subtract-rationals
  (is (= (sub (make-rat 1 2) (make-rat 2 3)) (make-rat -1 6))))

(deftest can-multiply-rationals
  (is (= (mul (make-rat 1 2) (make-rat 2 3)) (make-rat 1 3))))

(deftest can-divide-rationals
  (is (= (div (make-rat 1 2) (make-rat 2 3)) (make-rat 3 4))))
