(ns sicp.chapter-2.arithmetic.real-numbers
  (:use sicp.chapter-2.arithmetic.universal-arithmetic)
  (:use sicp.chapter-2.arithmetic.rational-numbers))

(defmethod add ::real [a b] (+ a b))
(defmethod sub ::real [a b] (- a b))
(defmethod mul ::real [a b] (* a b))
(defmethod div ::real [a b] (/ a b))
(defmethod equ? ::real [a b] (= a b))

(defmethod raise :sicp.chapter-2.arithmetic.rational-numbers/rational [a] (double (/ (numer a) (denom a))))
(derive :sicp.chapter-2.arithmetic.rational-numbers/rational ::real)
(defmethod get-type-tag Double [a] ::real)
