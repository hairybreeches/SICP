(ns sicp.test.pairs
  (:use sicp.chapter-2.pairs)
  (:use clojure.test))


(defn test-pairs[cons-impl car-impl cdr-impl]
  (let [pair (cons-impl 3 7)]
    (is (= (car-impl pair) 3))
    (is (= (cdr-impl pair) 7))))

(deftest test-functional-pairs
  (test-pairs func-cons func-car func-cdr))

(deftest test-numerical-pairs
  (test-pairs num-cons num-car num-cdr))
