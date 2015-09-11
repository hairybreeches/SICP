(ns sicp.chapter-2.arithmetic.real-numbers
  (:use clojure.math.numeric-tower)
  (:use sicp.chapter-2.arithmetic.universal-arithmetic)
  (:use sicp.chapter-2.arithmetic.rational-numbers))

(defmethod add-pair ::real [a b] (+ a b))
(defmethod mul-pair ::real [a b] (* a b))
(defmethod div-pair ::real [a b] (/ a b))
(defmethod equ? ::real [a b] (< (abs (- a b)) 0.000001))

(defmethod raise :sicp.chapter-2.arithmetic.rational-numbers/rational [a] (double (/ (numer a) (denom a))))
;note that this is a very rough: only works on terminating decimals with < 5 decimal digits
(defmethod number-project ::real [a] (make-rat (long (* 10000 a)) 10000))
(derive :sicp.chapter-2.arithmetic.rational-numbers/rational ::real)
(defmethod get-type-tag Double [a] ::real)
