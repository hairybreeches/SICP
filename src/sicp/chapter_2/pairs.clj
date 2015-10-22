(ns sicp.chapter-2.pairs
  (:use clojure.math.numeric-tower))

(defn cons-pair [x y]
  (ref #(% x y)))

(defn car [pair]
  (@pair (fn [p q] p)))

(defn cdr [pair]
  (@pair (fn [p q] q)))



(defn num-cons[x y]
  (* (expt 2 x)
     (expt 3 y)))

(defn exponent-of [n factor]
  (loop [value n
        exponent 0]
    (if (= (rem value factor) 0)
        (recur (/ value factor) (inc exponent))
        exponent)))

(defn num-car[pair]
  (exponent-of pair 2))

(defn num-cdr[pair]
  (exponent-of pair 3))
