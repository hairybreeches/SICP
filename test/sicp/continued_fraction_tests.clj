(ns sicp.continued-fraction-tests
  (:use sicp.test-accuracy)
  (:use clojure.math.numeric-tower)
  (:use clojure.test)
  (:use sicp.1-37))

(def reciprocal-phi 0.61803398875)

(defn approximate-reciprocal-phi [number-of-terms]
  (cont-frac (constantly 1) (constantly 1) number-of-terms))

(deftest calculate-reciprocal-phi
  (is-roughly=
   (approximate-reciprocal-phi 10)
   reciprocal-phi
   4))
