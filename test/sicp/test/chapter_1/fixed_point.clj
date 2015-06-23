(ns sicp.test.chapter-1.fixed-point
  (:use sicp.chapter-1.ex-35)
  (:use sicp.test.accuracy)
  (:use clojure.test))

(deftest calculate-phi
  (is-roughly= (fixed-point #(+ 1 (/ 1 %)) 1 0.0001) 1.61803398875, 0.0001))
