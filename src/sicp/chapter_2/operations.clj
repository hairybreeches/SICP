(ns sicp.chapter-2.operations
  (:use clojure.math.numeric-tower))


(defmulti get-record (fn [records id] (records :record-lookup)))
(defmulti get-salary (fn [record] (record :field-lookup)))

;map structure
(defmethod get-record :hashmap [records id] (records id))
(defmethod get-salary :hashmap [record] (record :salary))

;list structure
(defmethod get-record :list [records id] (first (filter #(= (first (% :record)) id) (records :records))))
(defmethod get-salary :list [record] (nth (:record record) 2))

(defn find-employee[id files]
  (first (filter #(not (nil? %)) (map #(get-record % id) files))))

(defn make-from-real-imag[x y]
  (defn dispatch[op]
    (cond (= op 'real-part) x
          (= op 'imag-part) y
          (= op 'magnitude) (sqrt (+ (expt x 2) (expt y 2)))
          (= op 'angle) (java.lang.Math/atan (/ y x))
          :else (throw (Exception. (str "Unknown operator " op " make-from-real-imag"))))))

(defn make-from-polar[magnitude angle]
  (defn dispatch[op]
    (cond (= op 'real-part) (* magnitude (java.lang.Math/cos angle))
          (= op 'imag-part) (* magnitude (java.lang.Math/sin angle))
          (= op 'magnitude) magnitude
          (= op 'angle) angle
          :else (throw (Exception. (str "Unknown operator " op " make-from-real-imag"))))))

(defn apply-generic[op arg] (arg op))
