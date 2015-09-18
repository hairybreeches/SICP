(ns sicp.chapter-2.arithmetic.universal-arithmetic
  (:use clojure.set)
  (:use sicp.chapter-2.arithmetic.arithmetic-component-interface))

;type coercion

(defn max-type[a b]
  (cond (isa? a b) b
        (isa? b a) a
        :else (throw (Exception. (str "cannot resolve types: " a " and " b)))))

(defn make-same[& args]
  (let [target (reduce max-type (map get-type-tag args))]
    (map (partial convert-to-type target) args)))


(defn operation[pair-op & args]
  (simplify (reduce pair-op args)))

(def add (partial operation add-pair))
(def sub (partial operation sub-pair))
(def mul (partial operation mul-pair))
(def div (partial operation div-pair))

(defn nought?[a] (equ? a 0))

(defmethod equ? :sicp.chapter-2.arithmetic.arithmetic-component-interface/mixed [a b] (apply equ? (make-same a b)))
(defmethod add-pair :sicp.chapter-2.arithmetic.arithmetic-component-interface/mixed [a b] (apply add (make-same a b)))
(defmethod mul-pair :sicp.chapter-2.arithmetic.arithmetic-component-interface/mixed [a b] (apply mul (make-same a b)))
(defmethod div-pair :sicp.chapter-2.arithmetic.arithmetic-component-interface/mixed [a b] (apply div (make-same a b)))










