(ns sicp.fib-test
  (:use clojure.test)
  (:use sicp.1-19))

(deftest fib-0
	 (is (= (fib 0) 0)))

(deftest fib-1
	 (is (= (fib 1) 1)))

(deftest fib-2
	 (is (= (fib 2) 1)))

(deftest fib-3
	 (is (= (fib 3) 2)))

(deftest fib-4
	 (is (= (fib 4) 3)))

(deftest fib-5
	 (is (= (fib 5) 5)))

(deftest fib-6
	 (is (= (fib 6) 8)))

(deftest fib-7
	 (is (= (fib 7) 13)))
