(ns sicp.rational-numbers-test
  (:use clojure.test)
  (:use sicp.chapter-2.rational-numbers))

(defn is-rat-equal [rat x]
  (is (= (/ (numer rat) (denom rat)) x)))


(deftest should-be-able-to-add
  (is-rat-equal (add-rat (make-rat 1 3) (make-rat 1 3)) 2/3))

(deftest should-be-able-to-subtract
  (is-rat-equal (sub-rat (make-rat 2 3) (make-rat 1 2)) 1/6))

(deftest should-be-able-to-multiply
  (is-rat-equal (mul-rat (make-rat 2 3) (make-rat 3 4)) 1/2))

(deftest should-be-able-to-divide
  (is-rat-equal (div-rat (make-rat 2 3) (make-rat 4 3)) 1/2))

