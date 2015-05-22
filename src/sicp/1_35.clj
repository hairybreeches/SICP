(ns sicp.1-35
  (:use clojure.math.numeric-tower))

(defn fixed-point [f first-guess tolerance]
  (defn close-enough? [v1 v2]
    (< (abs (- v1 v2)) tolerance))
  (loop [guess first-guess]
    (let [next-guess (f guess)]
      (if (close-enough? guess next-guess)
        next-guess
        (recur next-guess)))))

