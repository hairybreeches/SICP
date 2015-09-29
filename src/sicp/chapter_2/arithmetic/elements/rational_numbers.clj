(ns sicp.chapter-2.arithmetic.elements.rational-numbers
  (:use sicp.chapter-2.arithmetic.arithmetic-operations)
  (:use sicp.chapter-2.arithmetic.numerical-type-system))


(defn make-rat [n d]
  (let [[nn dd] (reduce-quotient n d)]
    ^{:type ::rational}
    {:numerator nn :denominator dd}))

(defn numer [x]
  (:numerator x))

(defn denom [x]
  (:denominator x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (mul-rat x (make-rat (denom y) (numer y))))


(defn equ?-rat[a b]
  (equ? (mul (numer a) (denom b))
        (mul (denom a) (numer b))))


(defmethod add-pair ::rational [a b] (add-rat a b))
(defmethod mul-pair ::rational [a b] (mul-rat a b))
(defmethod div-pair ::rational [a b] (div-rat a b))
(defmethod equ? ::rational [a b] (equ?-rat a b))
(defmethod variables ::rational [a] (mapcat variables (list (numer a) (denom a))))
(defmethod greatest-common-divisor ::rational [a b] (throw (Exception. "Cannot calculate gcd for rational numbers")))
(defmethod reduce-quotient ::rational [a b] (throw (Exception. "Cannot reduce quotient for rational numbers")))
(defmethod raise Long [a] (make-rat a 1))
(defmethod number-project ::rational [a] (numer a))
(derive Long ::rational)
