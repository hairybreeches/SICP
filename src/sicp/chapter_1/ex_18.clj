(ns sicp.chapter-1.ex-18)

(defn double [x]
  (+ x x))

(defn halve [x]
  (/ x 2))


(defn times-iter [a-initial b-initial c-initial]
  (loop [a a-initial
         b b-initial
         c c-initial]

    (cond
      (= b 0)  0
      (even? b) (recur (double a) (halve b) c)
      :else (recur a (- b 1) (+ c a)))))

(times 5 5)
