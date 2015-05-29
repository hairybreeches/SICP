(ns sicp.test-smooth
  (:use sicp.1-42)
  (:use clojure.test))

(defn step [x]
  (if (< x 0) 0 1))

(deftest test-smooth-step
  (let [smooth-step (smooth step 0.01)]
    (is (= (smooth-step 0.01) 1))
    (is (= (smooth-step 0.1) 1))
    (is (= (smooth-step -0.01) 1/3))
    (is (= (smooth-step -0.1) 0))
    (is (= (smooth-step 0) 2/3))))
