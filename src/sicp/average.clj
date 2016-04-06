(ns sicp.average)

(defn average[& args]
  (/ (reduce + args) (count args)))
