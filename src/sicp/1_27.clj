(ns sicp.1-27
  (:use sicp.1-21)
  (:use sicp.1-23))

(defn carmichael? [n]
  (and
    (every? #(fermat-test-with % n) (range 1 n))
     (not (prime? n))))
