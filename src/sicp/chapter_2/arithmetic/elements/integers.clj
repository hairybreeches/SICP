(ns sicp.chapter-2.arithmetic.elements.integers
  (:use sicp.chapter-2.arithmetic.arithmetic-operations)
  (:use sicp.chapter-2.arithmetic.elements.rational-numbers)
  (:use clojure.math.numeric-tower))

(defmethod add-pair Long [a b] (+ a b))
(defmethod mul-pair Long [a b] (* a b))
(defmethod div-pair Long [a b] (make-rat a b))
(defmethod equ? Long [a b] (= a b))
(defmethod variables Long [a] '())

(defn signed-gcd [a b]
  (let [g (gcd a b)]
    (if (< b 0)
      (* -1 g)
      g)))

(defmethod greatest-common-divisor Long [a b] (signed-gcd a b))
(defmethod reduce-quotient Long [n d]
  (let [g (greatest-common-divisor n d)]
    [(/ n g) (/ d g)]))

