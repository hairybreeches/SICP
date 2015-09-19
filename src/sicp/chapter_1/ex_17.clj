(ns sicp.chapter-1.ex-17)

(defn double [x]
  (+ x x))

(defn halve [x]
  (/ x 2))


(defn times [a b]
  (cond
    (= b 0)  0
    (even? b) (times (double a) (halve b))
    :else (+ a (times a (- b 1)))))

(times 4 5)
