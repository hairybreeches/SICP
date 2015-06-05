(ns sicp.interval-arithmetic-test
  (:use sicp.chapter-2.interval-arithmetic)
  (:use clojure.test))

(defn is-interval-equal [interval lower upper]
  (is (= (lower-bound interval) lower))
  (is (= (upper-bound interval)) upper))

(deftest add-ranges-test
  (is-interval-equal
   (add-interval
    (make-interval 1 4)
    (make-interval 2 6))
   3
   10))

(deftest subtract-ranges-test
  (is-interval-equal
   (sub-interval
    (make-interval 1 4)
    (make-interval 2 6))
   -5
   2))

(deftest divide-ranges-test
  (is-interval-equal
   (div-interval
    (make-interval 1 4)
    (make-interval 2 6))
   1/6
   2))

(deftest mul-ranges-test-both-positive
  (is-interval-equal
   (mul-interval
    (make-interval 1 4)
    (make-interval 2 6))
   2
   24))

(deftest mul-ranges-test-both-negative
  (is-interval-equal
   (mul-interval
    (make-interval -1 -4)
    (make-interval -2 -6))
   2
   24))

(deftest mul-ranges-test-one-negative-one-positive
  (is-interval-equal
   (mul-interval
    (make-interval -1 -4)
    (make-interval 2 6))
   -24
   -2))

(deftest mul-ranges-test-one-positive-one-spans
  (is-interval-equal
   (mul-interval
    (make-interval -1 4)
    (make-interval 2 6))
   -6
   24))

(deftest mul-ranges-test-one-negative-one-spans
  (is-interval-equal
   (mul-interval
    (make-interval -1 4)
    (make-interval -2 -6))
   -24
   6))

(deftest mul-ranges-test-both-span
  (is-interval-equal
   (mul-interval
    (make-interval -1 4)
    (make-interval -2 6))
   -8
   24))




