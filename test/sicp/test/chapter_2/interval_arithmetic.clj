(ns sicp.test.chapter-2.interval-arithmetic
  (:use sicp.chapter-2.interval-arithmetic)
  (:use clojure.test)
  (:use sicp.test.accuracy))

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
    (is (= (lower-bound interval) 1))
    (is (= (upper-bound interval) 3))
    (is (= (centre interval) 2))
    (is (= (width interval) 1))
    (is (= (percent-tolerance interval) 50))))

(deftest centre-percent-test
  (let [interval (make-centre-percent 4 25)]
    (is (= (lower-bound interval) 3))
    (is (= (upper-bound interval) 5))
    (is (= (centre interval) 4))
    (is (= (width interval) 1))
    (is (= (percent-tolerance interval) 25))))


(deftest lem-is-right
  (let [r1 (make-centre-percent 3 5)
        r2 (make-centre-percent 6 3)
        par1-result (par1 r1 r2)
        par2-result (par2 r1 r2)]
    (is (not= (centre par1-result) (centre par2-result)))
    (is-roughly= (centre par1-result) 2 1)
    (is-roughly= (centre par2-result) 2 3)
    (is-roughly= (percent-tolerance par1-result) 11.6 1)
    (is-roughly= (percent-tolerance par2-result) 4.3 1)))

(deftest dividing-an-interval-by-itself
  (let [interval (make-centre-percent 4 2)
        unity (div-interval interval interval)]
    (is-roughly= (centre unity) 1 2)
    (is-roughly= (percent-tolerance unity) 4 1)))






