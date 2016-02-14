(ns sicp.chapter-2.arithmetic.numerical-type-system)

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

(defn get-format[object]
  (:format (meta object)))
