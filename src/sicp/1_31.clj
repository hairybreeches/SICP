(ns sicp.1-31)

(defn product [term start iterator finish]
  (loop [start start
         result 1]
    (if (> start finish)
      result
      (recur (iterator start) (* (term start) result)))))


(defn factorial [n]
  (product identity 1 inc n))
