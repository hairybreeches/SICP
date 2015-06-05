(ns sicp.plane-geometry-test
  (:use sicp.chapter-2.plane-geometry)
  (:use clojure.test))

(defn is-point-equal[point x y]
  (is (= (x-point point) x))
  (is (= (y-point point)) y))


(deftest can-find-midpoint
  (is-point-equal (midpoint (make-segment (make-point -1 4) (make-point 2 3))) 1/2 7/2))

