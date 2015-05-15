(ns sicp.1-23
  (:use sicp.1-21)
  (:use sicp.1-19))

(defn rand-long[n]
  (long (* (rand) n)))

(defn rand-test-value [n]
  (+ 1 (rand-long (- n 1))))

(defn mod-square [n base]
  (rem (square n) base))

(defn expmod [base exp m]
    (cond (= exp 0) 1
        (even? exp) (mod-square (expmod base (/ exp 2) m) m)
        :else (rem (*'  base (expmod base (- exp 1) m)) m)))

(defn fermat-test-with [a n]
   (= (expmod a n n) a))

(defn fermat-prime? [n times]
  (every?
    #(fermat-test-with % n)
    (take times (repeatedly #(rand-test-value n)))))
