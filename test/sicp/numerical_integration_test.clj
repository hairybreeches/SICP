(ns sicp.numerical-integration-test
  (:use clojure.test)
  (:use sicp.1-29)
  (:use clojure.math.numeric-tower))

(defn is-roughly= [a b accuracy]
  (is (< (abs (- b a)) (/ 1 (expt 10 accuracy)))))

(deftest numerical-integration
  (is-roughly= (integral cube 0 1 0.01) 0.25 4)
  (is-roughly= (integral cube 0 1 0.001) 0.25 6))

(deftest simpson-integration
  (is-roughly= (simpson-integral cube 0 1 100) 0.25 8)
  (is-roughly= (simpson-integral cube 0 1 1000) 0.25 10))
