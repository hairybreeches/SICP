(ns sicp.test.smooth
  (:use sicp.chapter-1.ex-42)
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


(deftest test-3-smooth-step
  (let [smooth-step-3 (n-smooth step 3 0.01)]
    (is (= (smooth-step-3 0.1) 1))
    (is (= (smooth-step-3 0.03) 26/27))
    (is (= (smooth-step-3 0.02) 26/27))
    (is (= (smooth-step-3 0.01) 23/27))
    (is (= (smooth-step-3 0) 17/27))
    (is (= (smooth-step-3 -0.01) 10/27))
    (is (= (smooth-step-3 -0.02) 4/27))
    (is (= (smooth-step-3 -0.03) 1/27))
    (is (= (smooth-step-3 -0.1) 0))))
