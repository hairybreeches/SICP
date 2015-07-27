(ns sicp.test.chapter-2.sets
  (:use sicp.chapter-2.sets)
  (:use clojure.test))

(deftest element-of-set-true
  (are [element-of-set? make-set] (element-of-set? 2 (make-set 4 6 2))
       ul-element-of-set? ul-make-set))

(deftest element-of-set-false
  (are [element-of-set? make-set] (not (element-of-set? 2 (make-set 4 6 3)))
       ul-element-of-set? ul-make-set))

(deftest adjoin
  (are [set= adjoin-set make-set] (set= (adjoin-set 1 (make-set 2 3 4)) (make-set 1 2 3 4))
      ul-set= ul-adjoin-set ul-make-set))

(deftest intersection
  (are [set= intersection-set make-set] (set= (intersection-set (make-set 2 3 4) (make-set 5 4 3)) (make-set 3 4))
       ul-set= ul-intersection-set ul-make-set))


(deftest union
  (are [set= union-set make-set] (set= (union-set (make-set 1 5 4) (make-set  3 2 4 6)) (apply make-set (range 1 7)))
       ul-set= ul-union-set ul-make-set))
