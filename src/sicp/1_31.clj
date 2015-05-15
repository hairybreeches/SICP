(ns sicp.1-31)

(defn product [term start iterator finish]
  (defn iter[start result]
    (if (> start finish)
      result
      (iter (iterator start) (* (term start) result))))
  (iter start 1))


(defn factorial [n]
  (product identity 1 inc n))
