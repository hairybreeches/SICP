(ns sicp.sequences)

(defn non-empty-seq? [e]
  (and (seq? e)
       (not (empty? e))))
