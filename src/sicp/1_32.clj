(ns sicp.1-32)

(defn filtered-accumulate [predicate combiner null-value term start iterator finish]
  (loop [start start
         result null-value]
    (if (> start finish)
      result
      (recur
         (iterator start)
         (let [this-term (term start)]
           (if (predicate this-term)
               (combiner this-term result)
               result))))))

(defn accumulate [combiner null-value term start iterator finish]
  (filtered-accumulate (fn [x] true) combiner null-value term start iterator finish))


