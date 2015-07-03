(ns sicp.test.assertions
  (:use clojure.test)
  (:use clojure.set)
  (:use clojure.math.numeric-tower))

(defn roughly=[a b accuracy]
 (< (abs (- b a)) (/ 1 (expt 10 accuracy))))

(defn is-roughly= [a b accuracy]
  (is (roughly= (float a) (float b) accuracy)))

(defn is-set= [actual expected]
  (let [unexpected-elements (difference actual expected)
        missing-elements (difference expected actual)]
    (is (empty? unexpected-elements))
    (is (empty? missing-elements))))
