(ns sicp.1-42)


(defn compose [f g]
  #(f (g %)))
