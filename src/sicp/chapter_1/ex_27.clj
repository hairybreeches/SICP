(ns sicp.chapter-1.ex-27
  (:use sicp.chapter-1.ex-21)
  (:use sicp.chapter-1.ex-23))

(defn carmichael? [n]
  (and
    (every? #(fermat-test-with % n) (range 1 n))
     (not (prime? n))))
