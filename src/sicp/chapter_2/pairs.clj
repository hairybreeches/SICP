(ns sicp.chapter-2.pairs
  (:use clojure.math.numeric-tower))

(defn func-cons [x y]
  #(% x y))

(defn func-car [pair]
  (pair (fn [p q] p)))

(defn func-cdr [pair]
  (pair (fn [p q] q)))

(defn pair? [obj]
  (if (= (type obj) clojure.lang.Ref)
      (= (type @obj) ::pair)
      false))

(defn- cons-pair-value
  [p q]
  ^{:type ::pair}
  {:car p :cdr q})

(defn cons-pair
  [p q]
  (ref (cons-pair-value p q)))

(defn- car-value
  [pair-value]
  (pair-value :car))

(defn car
  [pair]
  (car-value @pair))

(defn- cdr-value
  [pair-value]
  (pair-value :cdr))

(defn cdr
  [pair]
  (cdr-value @pair))

(defn set-cdr!
  [pair new-cdr]
  (dosync
   (alter pair #(cons-pair-value (car-value %) new-cdr))))

(defn set-car!
  [pair new-car]
  (dosync
    (alter pair #(cons-pair-value new-car (cdr-value %)))))

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
