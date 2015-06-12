(ns sicp.test-accuracy
  (:use clojure.test)
  (:use clojure.math.numeric-tower))

(defn roughly=[a b accuracy]
 (< (abs (- b a)) (/ 1 (expt 10 accuracy))))

(defn is-roughly= [a b accuracy]
  (is (roughly= (float a) (float b) accuracy)))
