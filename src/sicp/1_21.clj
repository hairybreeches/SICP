(ns sicp.1-21
  (:use sicp.1-19))

(defn divides? [a b]
  (= (rem b a) 0))

(defn next[n]
  (if (= n 2) 3 (+ n 2)))



(defn find-divisor [n test-divisor]
  (loop [n n
         test-divisor test-divisor]
    (cond (> (square test-divisor) n) n
          (divides? test-divisor n) test-divisor
          :else (recur n (next test-divisor)))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(smallest-divisor 199) ;199

(smallest-divisor 1999) ; 1999

(smallest-divisor 199999) ; 199999









