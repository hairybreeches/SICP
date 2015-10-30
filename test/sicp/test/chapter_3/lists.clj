(ns sicp.test.chapter-3.lists
  (:use sicp.chapter-2.pairs)
  (:use sicp.chapter-3.lists)
  (:use clojure.test))


(def pair-list (let [pair-3 (cons-pair 3 nil)
                     pair-2 (cons-pair 2 pair-3)
                     pair-1 (cons-pair 1 pair-2)]
                 pair-1))

(def pair-cycle (let [pair-3 (cons-pair 3 nil)
                      pair-2 (cons-pair 2 pair-3)
                      pair-1 (cons-pair 1 pair-2)]
                     (set-cdr! pair-3 pair-1)
             pair-1))

(def repeated-pair-list (let [pair-3 (cons-pair 3 nil)
                              pair-2 (cons-pair 2 pair-3)
                              pair-1 (cons-pair pair-2 pair-3)]
                 pair-1))


(def tree (let [pair-3 (cons-pair nil nil)
                pair-2 (cons-pair pair-3 pair-3)
                pair-1 (cons-pair pair-2 pair-2)]
                 pair-1))


(deftest naive-count-fails
  (is (= (naive-count-pairs pair-list) 3))
  (is (= (naive-count-pairs repeated-pair-list) 4))
  (is (= (naive-count-pairs tree) 7)))

(deftest count-list
  (is (= (count-pairs pair-list) 3)))

(deftest count-repeated-list
  (is (= (count-pairs repeated-pair-list) 3)))

(deftest count-tree
  (is (= (count-pairs tree) 3)))

(deftest count-cycle
  (is (= (count-pairs pair-cycle) 3)))

