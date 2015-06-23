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

(defn segment-length [segment]
  (distance (start-segment segment)
            (end-segment segment)))

(defn point-average [point1 point2]
  (make-point (average (x-point point1) (x-point point2))
              (average (y-point point1) (y-point point2))))

(defn midpoint [segment]
  (point-average (start-segment segment)
                 (end-segment segment)))


;define a rectangle using the segments representing the height and width
(defn make-rectangle [segment1 segment2]
  [segment1 segment2])

(defn width [rectangle]
  (segment-length (first rectangle)))

(defn height [rectangle]
  (segment-length (second rectangle)))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))


(defn perimeter [rectangle]
  (* 2
    (+ (width rectangle)
       (height rectangle))))




