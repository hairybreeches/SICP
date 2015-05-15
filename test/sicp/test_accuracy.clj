(ns sicp.test-accuracy
  (:use clojure.test)
  (:use clojure.math.numeric-tower))

(defn is-roughly= [a b accuracy]
  (is (< (abs (- b a)) (/ 1 (expt 10 accuracy)))))
