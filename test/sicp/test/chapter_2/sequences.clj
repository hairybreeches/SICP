(ns sicp.test.chapter-2.sequences
  (:use sicp.chapter-2.sequences)
  (:use clojure.test))


(deftest last-item--in-single-element-sequence
  (is (= :a (last-pair [:a]))))

(deftest last-item--in-several-element-sequence
  (is (= 6 (last-pair '(1 2 3 4 6)))))
