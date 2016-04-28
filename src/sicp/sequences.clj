(ns sicp.sequences
  (:use sicp.error))

(defn non-empty-seq? [e]
  (and (seq? e)
       (not (empty? e))))

(defn- process-dots
  [r]
  (cond (not (= (first r) '.)) r
        (= (count r) 2) (second r)
        :else (error "cannot use dot notation with more than one subsequent: " r)))


(defn scheme-rest [s]
  (process-dots (rest s)))

