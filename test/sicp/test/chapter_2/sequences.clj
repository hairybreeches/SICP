(ns sicp.test.chapter-2.sequences
  (:use sicp.chapter-2.sequences)
  (:use clojure.test))


(deftest last-item-in-single-element-sequence
  (is (= :a (last-pair [:a]))))

(deftest last-item-in-several-element-sequence
  (is (= 6 (last-pair '(1 2 3 4 5 6)))))

(deftest reverse-empty-sequence
  (is (empty? (reverse-impl '()))))

(deftest reverse-single-element-sequence
  (is (= '(:zi) (reverse-impl '(:zi)))))

(deftest reverse-several-element-sequence
  (is (= '(6 5 4 3 2 1) (reverse-impl '(1 2 3 4 5 6)))))

(deftest count-change-book-example
  (is (= 292 (count-change 100 us-coins))))

