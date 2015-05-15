(ns sicp.1-32)

(defn accumulate-recursive [combiner null-value term start iterator finish]
    (if (> start finish)
      null-value
      (combiner (term start) (accumulate-recursive combiner null-value term (iterator start)  iterator finish))))



(defn accumulate [combiner null-value term start iterator finish]
  (loop [start start
         result null-value]
    (if (> start finish)
      result
      (recur (iterator start) (combiner (term start) result)))))


