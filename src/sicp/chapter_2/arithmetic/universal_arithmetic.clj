(ns sicp.chapter-2.arithmetic.universal-arithmetic
  (:use clojure.set))

;type coercion
(defmulti get-type-tag (fn [a] (type a)))
(defmethod get-type-tag :default [a] (type a))

(defmulti raise (fn [a] (get-type-tag a)))
(defmulti number-project (fn [a] (get-type-tag a)))

(defn max-type[a b]
  (cond (isa? a b) b
        (isa? b a) a
        :else (throw (Exception. (str "cannot resolve types: " a " and " b)))))

(defn convert-to-type[target object]
  (loop [object object]
    (if (= (get-type-tag object) target)
        object
        (recur (raise object)))))

(defn make-same[& args]
  (let [target (reduce max-type (map get-type-tag args))]
    (map (partial convert-to-type target) args)))

(defn type-lookup[& args]
  (if (apply = (map get-type-tag args))
      (get-type-tag (first args))
      ::mixed))

;interface methods
(defmulti equ? (fn [a b] (type-lookup a b)))
(defmethod equ? ::mixed [a b] (apply equ? (make-same a b)))

(defn simplify [a]
  (if (= (type a) Long)
      a
      (let [projected (number-project a)]
        (if (equ? a projected)
          (simplify projected)
          a))))

(defmulti add-pair (fn [a b] (type-lookup a b)))
(defmulti mul-pair (fn [a b] (type-lookup a b)))
(defn negate [a] (mul-pair -1 a))
(defn sub-pair [a b] (add-pair a (negate b)))
(defmulti div-pair (fn [a b] (type-lookup a b)))

(defn operation[pair-op & args]
  (simplify (reduce pair-op args)))

(def add (partial operation add-pair))
(def sub (partial operation sub-pair))
(def mul (partial operation mul-pair))
(def div (partial operation div-pair))

(defn nought?[a] (equ? a 0))

(defmethod add-pair ::mixed [a b] (apply add (make-same a b)))
(defmethod mul-pair ::mixed [a b] (apply mul (make-same a b)))
(defmethod div-pair ::mixed [a b] (apply div (make-same a b)))










