(ns sicp.1-41)

(defn do-twice [f]
  #(f (f %)))
