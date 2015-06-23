(ns sicp.chapter-1.ex-16)

(defn even? [n]
    (= 0 (rem n 2)))

(defn square [n]
    (* n n))

(defn expt-iter [a-initial b-initial n-initial]
  (loop [a a-initial
         b b-initial
         n n-initial]
    (cond (= n 0) a
        (even? n) (recur a (square b) (/ n 2))
        :else (recur (* a b) b (- n 1)))))

(defn expt [b n]
  (expt-iter 1 b n))

(expt 2 3)
