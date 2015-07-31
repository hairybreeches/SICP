(ns sicp.test.chapter-2.sets
  (:use sicp.chapter-2.sets)
  (:use clojure.test))

(deftest element-of-set-true
  (are [element-of-set? make-set] (element-of-set? 2 (make-set 4 6 2))
       ul-element-of-set? ul-make-set
       ol-element-of-set? ol-make-set
       ul-repeat-element-of-set? ul-repeat-make-set
       tree-element-of-set? tree-make-set))

(deftest element-of-set-false
  (are [element-of-set? make-set] (not (element-of-set? 2 (make-set 4 6 3)))
       ul-element-of-set? ul-make-set
       ol-element-of-set? ol-make-set
       ul-repeat-element-of-set? ul-repeat-make-set
       tree-element-of-set? tree-make-set))

(deftest adjoin-adds
  (are [set= adjoin-set make-set] (set= (adjoin-set 1 (make-set 2 3 4)) (make-set 1 2 3 4))
      ul-set= ul-adjoin-set ul-make-set
      ol-set= ol-adjoin-set ol-make-set
      ul-repeat-set= ul-repeat-adjoin-set ul-make-set
      tree-set= tree-adjoin-set tree-make-set))

(deftest adjoin-ignores
  (are [set= adjoin-set make-set] (set= (adjoin-set 1 (make-set 1 2 3 4)) (make-set 1 2 3 4))
      ul-set= ul-adjoin-set ul-make-set
      ol-set= ol-adjoin-set ol-make-set
      ul-repeat-set= ul-repeat-adjoin-set ul-make-set
      tree-set= tree-adjoin-set tree-make-set))

(deftest intersection
  (are [set= intersection-set make-set] (set= (intersection-set (make-set 2 3 4) (make-set 5 4 3)) (make-set 3 4))
       ul-set= ul-intersection-set ul-make-set
       ol-set= ol-intersection-set ol-make-set
       ul-repeat-set= ul-repeat-intersection-set ul-repeat-make-set
       tree-set= tree-intersection-set tree-make-set))


(deftest union
  (are [set= union-set make-set] (set= (union-set (make-set 1 5 4) (make-set  3 2 4 6)) (apply make-set (range 1 7)))
       ul-set= ul-union-set ul-make-set
       ol-set= ol-union-set ol-make-set
       ul-repeat-set= ul-repeat-union-set ul-repeat-make-set
       tree-set= tree-union-set tree-make-set))


;trees
;the example trees from diagram 2-16
(def tree-2-16-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(def tree-2-16-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(def tree-2-16-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(deftest list-program-1-orders-elements
  (is (= '(1 3 5 7 9 11) (tree->list-1 tree-2-16-1)))
  (is (= '(1 3 5 7 9 11) (tree->list-1 tree-2-16-2)))
  (is (= '(1 3 5 7 9 11) (tree->list-1 tree-2-16-3))))

(deftest list-program-2-orders-elements
  (is (= '(1 3 5 7 9 11) (tree->list-2 tree-2-16-1)))
  (is (= '(1 3 5 7 9 11) (tree->list-2 tree-2-16-2)))
  (is (= '(1 3 5 7 9 11) (tree->list-2 tree-2-16-3))))

(deftest list-to-tree
  (is (= '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))) (list->tree '(1 3 5 7 9 11)))))

