(ns sicp.test.chapter-1.filtered-accumulate
  (:use clojure.test)
  (:use sicp.chapter-1.ex-32))


(deftest test-sum-of-primes
  (is (= (sum-of-primes 5 29) 124)))


(deftest test-product-of-coprime
  (is (= (product-of-coprime 5) 24))
  (is (= (product-of-coprime 6) 5))
  (is (= (product-of-coprime 12) 385)))


