(ns sicp.chapter-2.rational-numbers
  (:use clojure.math.numeric-tower))


(defn signed-gcd [a b]
  (let [g (gcd a b)]
    (if (< b 0)
      (* -1 g)
      g)))

(defn make-rat [n d]
  (let [g (signed-gcd n d)]
    [(/ n g) (/ d g)]))

(defn numer [x]
  (first x))

(defn denom [x]
  (second x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (add-rat x (make-rat (* -1 (numer y)) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (mul-rat x (make-rat (denom y) (numer y))))
