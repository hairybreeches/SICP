(ns sicp.chapter-2.arithmetic.elements.rational-numbers
  (:use clojure.math.numeric-tower)
  (:use sicp.chapter-2.arithmetic.arithmetic-operations)
  (:use sicp.chapter-2.arithmetic.numerical-type-system))


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

(defmethod raise Long [a] (make-rat a 1))
(defmethod number-project ::rational [a] (numer a))
(derive Long ::rational)
