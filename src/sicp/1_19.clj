(ns sicp.1-19)

;p' = p^2 + q^2
;q' = q^2 + 2pq

(defn square [n]
  (*' n n))

(defn pprime [p q]
  (+ (square p) (square q)))

(defn qprime [p q]
  (+ (square q) (* 2 p q)))


(defn fib-iter [a b p q count]
  (loop
    [a a
    b b
    p p
    q q
    count count]

    (cond (= count 0) b
          (even? count) (recur a b (pprime p q) (qprime p q) (/ count 2))
          :else (recur
                  (+ (* b q) (* a q) (* a p))
                  (+ (* b p) (* a q))
                  p
                  q
                 (- count 1)))))

(defn fib [n]
  (fib-iter 1 0 0 1 n))

