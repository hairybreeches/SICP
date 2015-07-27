(ns sicp.test.chapter-2.sets
  (:use sicp.chapter-2.sets)
  (:use clojure.test))

(deftest element-of-set-true
  (is (ul-element-of-set? 2 (ul-make-set 4 6 2))))

(deftest element-of-set-false
  (is (not (ul-element-of-set? 2 (ul-make-set 4 6 3)))))

(deftest adjoin-set
  (is (ul-set= (ul-adjoin-set 1 (ul-make-set 2 3 4)) (ul-make-set 1 2 3 4))))

(deftest intersection-set
  (is (ul-set= (ul-intersection-set (ul-make-set 2 3 4) (ul-make-set 5 4 3)) (ul-make-set 3 4))))


(deftest union-set
  (is (ul-set= (ul-union-set (ul-make-set 1 5 4) (ul-make-set  3 2 4 6)) (apply ul-make-set (range 1 7)))))
