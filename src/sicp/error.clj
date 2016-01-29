(ns sicp.error)

(defn error[& args]
  (throw (Exception. (apply str args))))
