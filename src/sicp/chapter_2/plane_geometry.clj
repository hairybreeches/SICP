(ns sicp.chapter-2.plane-geometry
  (:use sicp.average))

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

(defn point-average [point1 point2]
  (make-point (average (x-point point1) (x-point point2))
              (average (y-point point1) (y-point point2))))

(defn midpoint [segment]
  (point-average (start-segment segment)
                 (end-segment segment)))




