(ns sicp.test.continued-fractions
  (:use sicp.test.accuracy)
  (:use clojure.math.numeric-tower)
  (:use clojure.test)
  (:use sicp.chapter-1.ex-37))

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


(defn tan-cf [x number-of-terms]
  (let [negative-x-squared (* -1 x x)]
    (cont-frac
     #(if (= % 1) x negative-x-squared)
     #(- (* 2 %) 1)
     number-of-terms)))

(def pi java.lang.Math/PI)

(deftest test-tan-0
  (is-roughly= (tan-cf 0 10) 0 10))

(deftest test-tan-pi
  (is-roughly= (tan-cf pi 15) 0 10))

(deftest test-tan-quarter-pi
  (is-roughly= (tan-cf (/ pi 4) 10) 1 10))


