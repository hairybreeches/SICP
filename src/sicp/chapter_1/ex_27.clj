(ns sicp.chapter-1.ex-27
  (:use sicp.chapter-1.ex-21)
  (:use sicp.chapter-1.primes))

(defn carmichael? [n]
  (and
    (every? #(fermat-test-with % n) (range 1 n))
     (not (prime? n))))
