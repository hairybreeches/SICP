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

(defn euler-denominator [n]
  (if (= (rem n 3) 2)
    (* 2 (/ (+ 1 n) 3))
    1))

(deftest test-euler-denominator
  (is (= (map euler-denominator (range 1 15)) [1 2 1 1 4 1 1 6 1 1 8 1 1 10])))

(defn approximate-e [number-of-terms]
  (+ 2 (cont-frac (constantly 1) euler-denominator number-of-terms)))

(def e 2.718281828459045)

(deftest calculate-e
  (is-roughly= (approximate-e 15) e 10))

