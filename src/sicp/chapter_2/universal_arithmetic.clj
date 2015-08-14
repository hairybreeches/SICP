(ns sicp.chapter-2.universal-arithmetic)

(defmulti add (fn [a b] (type a)))
(defmulti sub (fn [a b] (type a)))
(defmulti mul (fn [a b] (type a)))
(defmulti div (fn [a b] (type a)))
(defmulti equ? (fn [a b] (type a)))

(defn fail-lookup[operation & args]
  (throw (Exception. (str "Cannot resolve " operation " for type: "  (type (first args))))))


(defmethod add :default [a b] (fail-lookup "addition" a b))
(defmethod sub :default [a b] (fail-lookup "subtraction" a b))
(defmethod mul :default [a b] (fail-lookup "mutliplication" a b))
(defmethod div :default [a b] (fail-lookup "division" a b))

;inbuilt numbers
(defmethod add Long [a b] (+ a b))
(defmethod sub Long [a b] (- a b))
(defmethod mul Long [a b] (* a b))
(defmethod div Long [a b] (/ a b))
(defmethod equ? Long [a b] (= a b))







