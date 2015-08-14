(ns sicp.chapter-2.real-numbers
  (:use sicp.chapter-2.universal-arithmetic)
  (:use sicp.chapter-2.rational-numbers))

(defmethod add ::real [a b] (+ a b))
(defmethod sub ::real [a b] (- a b))
(defmethod mul ::real [a b] (* a b))
(defmethod div ::real [a b] (/ a b))
(defmethod equ? ::real [a b] (= a b))
(defmethod nought? ::real [a] (equ? a 0))

(defmethod raise :sicp.chapter-2.rational-numbers/rational [a] (float (/ (numer a) (denom a))))
(derive :sicp.chapter-2.rational-numbers/rational ::real)
(defmethod get-type-tag Float [a] ::real)
