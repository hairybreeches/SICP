(ns sicp.chapter-2.arithmetic.arithmetic-component-interface)

;basic type system interface

(defmulti get-type-tag (fn [a] (type a)))
(defmethod get-type-tag :default [a] (type a))

(defmulti raise (fn [a] (get-type-tag a)))
(defmulti number-project (fn [a] (get-type-tag a)))

(defn convert-to-type[target object]
  (loop [object object]
    (if (= (get-type-tag object) target)
        object
        (recur (raise object)))))

(defn type-lookup[& args]
  (if (apply = (map get-type-tag args))
      (get-type-tag (first args))
      ::mixed))

;arithmetic operations to implement
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
