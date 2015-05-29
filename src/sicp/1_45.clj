(ns sicp.1-45
  (:use sicp.1-35)
  (:use sicp.1-42)
  (:use sicp.average)
  (:use clojure.math.numeric-tower))

(defn average-damp [f]
  #(average (f %) %))


(defn find-root [n root-degree]
  ;this is actually over-zealous, damp-coeffiecient only has to be floor of log to base 2
  (let [damp-coefficient (floor (/ root-degree 2))
        damper (repeated average-damp damp-coefficient)
        original-function #(/ n (expt % (- root-degree 1)))
        damped-function #(int ((damper original-function) %))]
    (fixed-point damped-function 1 0.0001)))
