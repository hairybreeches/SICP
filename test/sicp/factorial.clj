(ns sicp.factorial
  (:use clojure.test)
  (:use sicp.1-31)
  (:use sicp.test-accuracy))


(deftest test-factorial
  (is (= (factorial 1) 1))
  (is (= (factorial 2) 2))
  (is (= (factorial 3) 6))
  (is (= (factorial 4) 24))
  (is (= (factorial 5) 120))
  (is (= (factorial 6) 720)))

(deftest test-pi
  (is (is-roughly= (float (pi 1000)) 3.14159 2)))
