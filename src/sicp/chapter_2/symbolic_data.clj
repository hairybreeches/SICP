(ns sicp.chapter-2.symbolic-data)

(defn sequence-equal[a b]
  (loop [a a
         b b]
    (cond (and (empty? a) (empty? b)) true
          (empty? a) false
          (empty? b) false
          (not= (first a) (first b)) false
          :else (recur (rest a) (rest b)))))

