(ns sicp.chapter-2.arithmetic.integers
  (:use sicp.chapter-2.arithmetic.universal-arithmetic))

(defmethod add Long [a b] (+ a b))
(defmethod sub Long [a b] (- a b))
(defmethod mul Long [a b] (* a b))
(defmethod div Long [a b] (/ a b))
(defmethod equ? Long [a b] (= a b))

