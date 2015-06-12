(ns sicp.test.chapter-2.rational-numbers
  (:use clojure.test)
  (:use sicp.chapter-2.rational-numbers))

(defn is-rat-equal [rat n d]
  (is (= (numer rat) n))
  (is (= (denom rat)) d))


(deftest should-be-able-to-add
  (is-rat-equal (add-rat (make-rat 1 3) (make-rat 1 3)) 2 3))

(deftest should-be-able-to-subtract
  (is-rat-equal (sub-rat (make-rat 2 3) (make-rat 1 2)) 1 6))

(deftest should-be-able-to-multiply
  (is-rat-equal (mul-rat (make-rat 2 3) (make-rat 3 4)) 1 2))

(deftest should-be-able-to-divide
  (is-rat-equal (div-rat (make-rat 2 3) (make-rat 4 3)) 1 2))


(deftest if-denominator-negative-and-numerator-positive
  (is-rat-equal (make-rat 2 -3) -2 3))

(deftest if-numerator-and-denominator-both-negative
  (is-rat-equal (make-rat -2 -6) 1 3))

(deftest if-numerator-and-denominator-both-positive
  (is-rat-equal (make-rat 7 14) 1 2))

