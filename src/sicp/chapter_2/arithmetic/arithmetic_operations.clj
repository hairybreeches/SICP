(ns sicp.chapter-2.arithmetic.arithmetic-operations
  (:use sicp.chapter-2.arithmetic.numerical-type-system))

(defmulti variables (fn [a] (type-lookup a)))

(defn get-variables[a]
  (sort (distinct (variables a))))

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

(defn add [& args] (apply (partial operation add-pair) args))
(defn sub [& args] (apply (partial operation sub-pair) args))
(defn mul [& args] (apply (partial operation mul-pair) args))
(defn div [& args] (apply (partial operation div-pair) args))

(defn nought?[a] (equ? a 0))
