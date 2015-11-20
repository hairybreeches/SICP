(ns sicp.test.chapter-3.lists
  (:use sicp.chapter-2.pairs)
  (:use sicp.chapter-3.lists)
  (:use clojure.test))


(def pair-list (make-list 3))

(def pair-cycle (make-cycle 3))

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

(deftest finds-cycle-odd
  (is (contains-cycle? (make-cycle 7))))

(deftest finds-cycle-even
  (is (contains-cycle? (make-cycle 6))))

(deftest finds-cycle-one-element
  (is (contains-cycle? (make-cycle 1))))

(deftest finds-cycle-two-element
  (is (contains-cycle? (make-cycle 2))))

(deftest finds-no-cycle-in-list
  (is (not (contains-cycle? (make-list 12)))))

(deftest finds-no-cycle-in-single-element-list
  (is (not (contains-cycle? (make-list 1)))))

(deftest finds-no-cycle-in-two-element-list
  (is (not (contains-cycle? (make-list 2)))))

(deftest finds-no-cycle-in-three-element-list
  (is (not (contains-cycle? (make-list 3)))))

(deftest finds-even-cycle-with-tail
    (is (contains-cycle? (make-cycle-with-tail 8 12))))

(deftest finds-odd-cycle-with-tail
    (is (contains-cycle? (make-cycle-with-tail 8 15))))
