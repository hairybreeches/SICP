(ns sicp.1-37)

(defn cont-frac [numerator-function denominator-function to-index]
  (defn iter [from-index]
    (if (> from-index to-index)
      0
      (/ (numerator-function from-index)
         (+ (denominator-function from-index) (iter (inc from-index))))))
  (iter 0))
