(ns sicp.chapter-1.ex-32
  (:use clojure.math.numeric-tower)
  (:use sicp.chapter-1.ex-21))

(defn filtered-accumulate [predicate combiner null-value term start iterator finish]
  (loop [start start
         result null-value]
    (if (> start finish)
      result
      (recur
         (iterator start)
         (let [this-term (term start)]
           (if (predicate this-term)
               (combiner this-term result)
               result))))))

(defn accumulate [combiner null-value term start iterator finish]
  (filtered-accumulate (fn [x] true) combiner null-value term start iterator finish))

(defn sum-of-primes [a b]
  (filtered-accumulate prime? + 0 identity a inc b))

(defn product-of-coprime [n]
  (filtered-accumulate #(= (gcd n %) 1) * 1 identity 1 inc n))


