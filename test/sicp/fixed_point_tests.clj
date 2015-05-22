(ns sicp.fixed-point-tests
  (:use sicp.1-35)
  (:use sicp.test-accuracy)
  (:use clojure.test))

(deftest calculate-phi
  (is-roughly= (fixed-point #(+ 1 (/ 1 %)) 1 0.0001) 1.61803398875, 0.0001))
