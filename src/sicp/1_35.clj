(ns sicp.1-35
  (:use sicp.1-46)
  (:use clojure.math.numeric-tower))

(defn fixed-point [f first-guess tolerance]
  (defn close-enough? [guess next-guess]
    (< (abs (- guess next-guess)) tolerance))
  ((iterative-improve close-enough? f) first-guess))

