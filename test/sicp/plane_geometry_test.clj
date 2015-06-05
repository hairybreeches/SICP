(ns sicp.plane-geometry-test
  (:use sicp.chapter-2.plane-geometry)
  (:use clojure.test))

(defn is-point-equal[point x y]
  (is (= (x-point point) x))
  (is (= (y-point point)) y))


(deftest can-find-midpoint
  (is-point-equal (midpoint (make-segment (make-point -1 4) (make-point 2 3))) 1/2 7/2))


(def test-rectangle
  (make-rectangle (make-point 0 0)
                  (make-point 0 5)
                  (make-point 2 5)
                  (make-point 2 0)))

(def rotated-test-rectangle
  (make-rectangle (make-point 0 0)
                  (make-point -8 6)
                  (make-point -5 10)
                  (make-point 3 4)))

(deftest calculate-distance-when-x-distance-is-0
  (is (= (distance (make-point 3 5)
                   (make-point 3 11))
          6)))

(deftest calculate-distance-when-y-distance-is-0
  (is (= (distance (make-point 4 5)
                   (make-point 1 5))
          3)))

(deftest calculate-distance-using-pythagoras
  (is (= (distance (make-point 4 2)
                   (make-point 7 -2))
          5)))

(deftest calculate-width
  (is (= (width test-rectangle) 5)))

(deftest calculate-height
  (is (= (height test-rectangle) 2)))

(deftest calculate-area
  (is (= (area test-rectangle) 10)))

(deftest calculate-rotated-area
  (is (= (area rotated-test-rectangle) 50)))

(deftest calculate-perimeter
  (is (= (perimeter test-rectangle) 14)))

(deftest calculate-rotated-perimeter
  (is (= (perimeter rotated-test-rectangle) 30)))

