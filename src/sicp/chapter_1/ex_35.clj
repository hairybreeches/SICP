(ns sicp.chapter-1.ex-35
  (:use sicp.chapter-1.ex-46)
  (:use clojure.math.numeric-tower))

(defn fixed-point [f first-guess tolerance]
  (defn close-enough? [guess next-guess]
    (< (abs (- guess next-guess)) tolerance))
  ((iterative-improve close-enough? f) first-guess))

