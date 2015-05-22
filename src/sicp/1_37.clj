(ns sicp.1-37)

(defn cont-frac [numerator-function denominator-function to-index]
  (loop [to-index to-index
         result 0]
    (if (= to-index 0)
      result
      (recur (dec to-index)
             (/ (numerator-function to-index) (+ (denominator-function to-index) result))))))
