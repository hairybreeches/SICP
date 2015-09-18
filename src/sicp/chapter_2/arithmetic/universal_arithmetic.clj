(ns sicp.chapter-2.arithmetic.universal-arithmetic
  (:require sicp.chapter-2.arithmetic.complex-numbers)
  (:require sicp.chapter-2.arithmetic.integers)
  (:require sicp.chapter-2.arithmetic.polynomials)
  (:require sicp.chapter-2.arithmetic.rational-numbers)
  (:require sicp.chapter-2.arithmetic.real-numbers)
  (:use sicp.chapter-2.arithmetic.arithmetic-operations)
  (:use sicp.chapter-2.arithmetic.numerical-type-system))


(defn max-type[a b]
  (cond (isa? a b) b
        (isa? b a) a
        :else (throw (Exception. (str "cannot resolve types: " a " and " b)))))

(defn make-same[& args]
  (let [target (reduce max-type (map get-type-tag args))]
    (map (partial convert-to-type target) args)))

(defmethod equ? :sicp.chapter-2.arithmetic.numerical-type-system/mixed [a b] (apply equ? (make-same a b)))
(defmethod add-pair :sicp.chapter-2.arithmetic.numerical-type-system/mixed [a b] (apply add (make-same a b)))
(defmethod mul-pair :sicp.chapter-2.arithmetic.numerical-type-system/mixed [a b] (apply mul (make-same a b)))
(defmethod div-pair :sicp.chapter-2.arithmetic.numerical-type-system/mixed [a b] (apply div (make-same a b)))










