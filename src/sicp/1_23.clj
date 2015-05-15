(ns sicp.1-23
  (:use sicp.1-21)
  (:use sicp.1-19))

(defn rand-long[n]
  (long (* (rand) n)))


(defn expmod [base exp m]
    (cond (= exp 0) 1
        (even? exp) (rem (square (expmod base (/ exp 2) m)) m)
        :else (rem (*'  base (expmod base (- exp 1) m)) m)))

(defn fermat-test-with [a n]
   (= (expmod a n n) a))

(defn fermat-test [n]
  (fermat-test-with (+ 1 (rand-long (- n 1))) n))

(defn carmichael? [n]
  (every? #(fermat-test-with % n) (range 1 n)))

(defn fast-prime? [n times]
  (loop [n n
         times times]
    (cond (= times 0) true
          (fermat-test n) (recur n (- times 1))
          :else false)))
