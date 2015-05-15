(ns sicp.1-31)

(defn product [term start iterator finish]
  (if (> start finish)
      1
      (* (term start) (product term (iterator start) iterator finish))))

(defn factorial [n]
  (product identity 1 inc n))
