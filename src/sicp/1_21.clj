(ns sicp.1-21
  (:use sicp.1-19))

(defn divides? [a b]
  (= (rem b a) 0))


(defn find-divisor [n test-divisor]
  (loop [n n
         test-divisor test-divisor]
    (cond (> (square test-divisor) n) n
          (divides? test-divisor n) test-divisor
          :else (recur n (+ test-divisor 1)))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(smallest-divisor 199) ;199

(smallest-divisor 1999) ; 1999

(smallest-divisor 199999) ; 199999


(defn report-if-prime [n]
    (print
       (if (prime? n)
         " is prime "
         " is not prime ")))

(defn timed-prime-test [n]
  (print "\n")
  (print n)
  (time (report-if-prime n)))

(defn show-prime-complexity []
  (timed-prime-test 1009)
  (timed-prime-test 10007)
  (timed-prime-test 100003)
  (timed-prime-test 1000003))









