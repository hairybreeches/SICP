(ns sicp.chapter-2.arithmetic.arithmetic-operations
  (:use sicp.chapter-2.arithmetic.numerical-type-system))

;pairwise operations
(defmulti add-pair (fn [a b] (type-lookup a b)))
(defmulti mul-pair (fn [a b] (type-lookup a b)))
(defn negate [a] (mul-pair -1 a))
(defn sub-pair [a b] (add-pair a (negate b)))
(defmulti div-pair (fn [a b] (type-lookup a b)))

(defmulti equ? (fn [a b] (type-lookup a b)))
(defn simplify [a]
  (if (= (type a) Long)
      a
      (let [projected (number-project a)]
        (if (equ? a projected)
          (simplify projected)
          a))))

(defn operation[pair-op & args]
  (simplify (reduce pair-op args)))

(def add (partial operation add-pair))
(def sub (partial operation sub-pair))
(def mul (partial operation mul-pair))
(def div (partial operation div-pair))

(defn nought?[a] (equ? a 0))
