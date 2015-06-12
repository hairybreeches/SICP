(ns sicp.chapter-2.sequences)

(defn last-pair[things]
  (loop [things things]
    (let [cdr (rest things)]
      (if (empty? cdr)
        (first things)
        (recur cdr)))))

(defn reverse-impl[things]
  (loop [so-far '()
         things things]
    (if (empty? things)
        so-far
        (recur (cons (first things) so-far) (rest things)))))

(def us-coins '(50 25 10 5 1))
(def uk-coins '(100 50 20 10 5 2 1 0.5))

(def except-first-denomination rest)
(def first-denomination first)
(def no-more? empty?)

(defn count-change[amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (count-change amount (except-first-denomination coin-values))
                 (count-change (- amount (first-denomination coin-values)) coin-values))))

