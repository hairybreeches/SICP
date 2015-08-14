(ns sicp.chapter-2.arithmetic.universal-arithmetic
  (:use clojure.set))

;type coercion
(defmulti get-type-tag (fn [a] (type a)))
(defmethod get-type-tag :default [a] (type a))

(defmulti raise (fn [a] (get-type-tag a)))

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
(defmulti add (fn [a b] (type-lookup a b)))
(defmulti sub (fn [a b] (type-lookup a b)))
(defmulti mul (fn [a b] (type-lookup a b)))
(defmulti div (fn [a b] (type-lookup a b)))
(defmulti equ? (fn [a b] (type-lookup a b)))
(defn nought?[a] (equ? a 0))

(defmethod add ::mixed [a b] (apply add (make-same a b)))
(defmethod sub ::mixed [a b] (apply sub (make-same a b)))
(defmethod mul ::mixed [a b] (apply mul (make-same a b)))
(defmethod div ::mixed [a b] (apply div (make-same a b)))
(defmethod equ? ::mixed [a b] (apply equ? (make-same a b)))










