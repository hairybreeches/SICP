(ns sicp.chapter-2.arithmetic.rational-numbers
  (:use clojure.math.numeric-tower)
  (:use sicp.chapter-2.arithmetic.universal-arithmetic))


(defn signed-gcd [a b]
  (let [g (gcd a b)]
    (if (< b 0)
      (* -1 g)
      g)))

(defn make-rat [n d]
  (let [g (signed-gcd n d)]
    ^{:type ::rational}
    {:numerator (/ n g) :denominator (/ d g)}))

(defn numer [x]
  (:numerator x))

(defn denom [x]
  (:denominator x))

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


(defn equ?-rat[a b]
  (equ? (mul (numer a) (denom b))
        (mul (denom a) (numer b))))

(defn nought?-rat[a]
  (equ?-rat a (make-rat 0 1)))

(defmethod add ::rational [a b] (add-rat a b))
(defmethod sub ::rational [a b] (sub-rat a b))
(defmethod mul ::rational [a b] (mul-rat a b))
(defmethod div ::rational [a b] (div-rat a b))
(defmethod equ? ::rational [a b] (equ?-rat a b))
(defmethod nought? ::rational [a] (nought?-rat a))

(defmethod raise Long [a] (make-rat a 1))
(derive Long ::rational)
