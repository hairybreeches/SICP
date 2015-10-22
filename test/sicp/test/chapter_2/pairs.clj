(ns sicp.test.chapter-2.pairs
  (:use sicp.chapter-2.pairs)
  (:use clojure.test))


(defn test-pairs[cons-impl car-impl cdr-impl]
  (let [pair (cons-impl 3 7)]
    (is (= (car-impl pair) 3))
    (is (= (cdr-impl pair) 7))))

(deftest test-functional-pairs
  (test-pairs cons-pair car cdr))

(deftest test-numerical-pairs
  (test-pairs num-cons num-car num-cdr))

(deftest can-mutate-car
  (let [pair (cons-pair 2 4)]
    (set-car! pair 3)
    (is (= (car pair) 3))
    (is (= (cdr pair) 4))))

(deftest can-mutate-cdr
  (let [pair (cons-pair 2 4)]
    (set-cdr! pair 8)
    (is (= (car pair) 2))
    (is (= (cdr pair) 8))))
