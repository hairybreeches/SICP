(ns sicp.chapter-1.primes
  (:use sicp.chapter-1.ex-19))

(defn divides? [a b]
  (= (rem b a) 0))

(defn next-divisor [n]
  (if (= n 2) 3 (+ n 2)))

(defn find-divisor [n test-divisor]
  (loop [n n
         test-divisor test-divisor]
    (cond (> (square test-divisor) n) n
          (divides? test-divisor n) test-divisor
          :else (recur n (next-divisor test-divisor)))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(defn rand-long[n]
  (long (* (rand) n)))

(defn rand-test-value [n]
  (+ 1 (rand-long (- n 1))))

(defn mod-square [n base]
  (rem (square n) base))

(defn miller-rabin-square [n base]
  (let [result (mod-square n base)]
    (if
      (and
        (not= 1 n)
        (not= (- base 1) n)
        (= result 1))
      0
      result)))

(defn expmod [base exp m squarer]
    (cond (= exp 0) 1
        (even? exp) (squarer (expmod base (/ exp 2) m squarer) m)
        :else (rem (*'  base (expmod base (- exp 1) m squarer)) m)))

(defn fermat-test-with
  ([a n] (fermat-test-with a n mod-square))
  ([a n squarer] (= (expmod a n n squarer) a)))

(defn multi-fermat-test [n times squarer]
  (every?
    #(fermat-test-with % n squarer)
    (take times (repeatedly #(rand-test-value n)))))

(defn fermat-prime? [n times]
  (multi-fermat-test n times mod-square))

(defn miller-rabin-prime? [n times]
  (multi-fermat-test n times miller-rabin-square))
