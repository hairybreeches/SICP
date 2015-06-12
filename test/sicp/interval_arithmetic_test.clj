(ns sicp.interval-arithmetic-test
  (:use sicp.chapter-2.interval-arithmetic)
  (:use clojure.test))

(defn is-interval-equal [interval lower upper]
  (is (= (lower-bound interval) lower))
  (is (= (upper-bound interval) upper)))

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
    (make-interval -4 -1)
    (make-interval -6 -2))
   2
   24))


(deftest mul-ranges-test-both-span
  (is-interval-equal
   (mul-interval
    (make-interval -1 4)
    (make-interval -2 6))
   -8
   24))

(deftest mul-ranges-test-a-negative-b-positive
  (is-interval-equal
   (mul-interval
    (make-interval -4 -1)
    (make-interval 2 6))
   -24
   -2))

(deftest mul-ranges-test-a-positive-b-negative
  (is-interval-equal
   (mul-interval
    (make-interval 2 6)
    (make-interval -4 -1))
   -24
   -2))

(deftest mul-ranges-test-a-spans-b-positive
  (is-interval-equal
   (mul-interval
    (make-interval -1 4)
    (make-interval 2 6))
   -6
   24))

(deftest mul-ranges-test-a-positive-b-spans
  (is-interval-equal
   (mul-interval
    (make-interval 2 6)
    (make-interval -1 4))
   -6
   24))

(deftest mul-ranges-test-a-spans-b-negative
  (is-interval-equal
   (mul-interval
    (make-interval -1 4)
    (make-interval -6 -2))
   -24
   6))

(deftest mul-ranges-test-a-negative-b-spans
  (is-interval-equal
   (mul-interval
    (make-interval -6 -2)
    (make-interval -1 4))
   -24
   6))

(deftest centre-width-test
  (let [interval (make-centre-width 2 1)]
    (is-interval-equal interval 1 3)
    (is (= (centre interval) 2))
    (is (= (width interval) 1))))




