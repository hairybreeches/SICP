(ns sicp.chapter-1.ex-42
  (:use sicp.average))


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

(defn smooth [f dx]
  #(average (f %) (f (+ % dx)) (f (- % dx))))


(defn n-smooth [f n dx]
  ((repeated #(smooth % dx) n) f))
