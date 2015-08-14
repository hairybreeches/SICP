(ns sicp.chapter-2.universal-arithmetic
  (:use sicp.chapter-2.rational-numbers)
  (:use sicp.chapter-2.complex-numbers))

(defmulti add (fn [a b] (map type [a b])))
(defmulti sub (fn [a b] (map type [a b])))
(defmulti mul (fn [a b] (map type [a b])))
(defmulti div (fn [a b] (map type [a b])))

(defn fail-lookup[operation & args]
  (throw (Exception. (apply str "Cannot resolve " operation " for types: "  (map type args)))))


(defmethod add :default [a b] (fail-lookup "addition" a b))
(defmethod sub :default [a b] (fail-lookup "subtraction" a b))
(defmethod mul :default [a b] (fail-lookup "mutliplication" a b))
(defmethod div :default [a b] (fail-lookup "division" a b))

;inbuilt numbers
(defmethod add [Long Long] [a b] (+ a b))
(defmethod sub [Long Long] [a b] (- a b))
(defmethod mul [Long Long] [a b] (* a b))
(defmethod div [Long Long] [a b] (/ a b))





