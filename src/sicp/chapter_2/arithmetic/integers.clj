(ns sicp.chapter-2.arithmetic.integers
  (:use sicp.chapter-2.arithmetic.arithmetic-component-interface))

(defmethod add-pair Long [a b] (+ a b))
(defmethod mul-pair Long [a b] (* a b))
(defmethod div-pair Long [a b] (/ a b))
(defmethod equ? Long [a b] (= a b))

