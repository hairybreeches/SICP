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

