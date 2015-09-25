(ns sicp.chapter-2.arithmetic.elements.integers
  (:use sicp.chapter-2.arithmetic.arithmetic-operations))

(defmethod add-pair Long [a b] (+ a b))
(defmethod mul-pair Long [a b] (* a b))
(defmethod div-pair Long [a b] (/ a b))
(defmethod equ? Long [a b] (= a b))
(defmethod variables Long [a] '())

