(ns sicp.chapter-2.plane-geometry
  (:use sicp.average)
  (:use clojure.math.numeric-tower))

(defn make-segment [start end]
  [start end])

(defn start-segment [segment]
  (first segment))

(defn end-segment [segment]
  (second segment))

(defn make-point [x y]
  [x y])

(defn x-point [point]
  (first point))

(defn y-point [point]
  (second point))

(defn distance[point1 point2]
  (sqrt (+ (expt (- (x-point point1)
                    (x-point point2))
                 2)
           (expt (- (y-point point1)
                    (y-point point2))
                 2))))

(defn point-average [point1 point2]
  (make-point (average (x-point point1) (x-point point2))
              (average (y-point point1) (y-point point2))))

(defn midpoint [segment]
  (point-average (start-segment segment)
                 (end-segment segment)))


(defn make-rectangle [point1 point2 point3 point4]
  [point1 point2 point3 point4])

(defn point1 [rectangle]
  (first rectangle))

(defn point2 [rectangle]
  (second rectangle))

(defn point3 [rectangle]
  (nth rectangle 2))

(defn point4 [rectangle]
  (nth rectangle 3))

(defn width [rectangle]
  (distance (point1 rectangle)
            (point2 rectangle)))

(defn height [rectangle]
  (distance (point2 rectangle)
            (point3 rectangle)))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))


(defn perimeter [rectangle]
  (* 2
    (+ (width rectangle)
       (height rectangle))))




