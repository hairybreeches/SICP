(ns sicp.1-42)


(defn compose [f g]
  #(f (g %)))

(defn repeated [f n]
  (loop [result identity
         f f
         n n]
    (cond
     (= n 0) result
     (= (rem n 2) 0) (recur result (compose f f) (/ n 2))
     :else (recur (compose f result) f (dec n)))))
