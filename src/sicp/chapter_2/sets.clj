(ns sicp.chapter-2.sets)

;sets as unorderered lists
(defn ul-element-of-set?[x a-set]
  (loop [x x
         a-set a-set]
    (cond (empty? a-set) false
          (= x (first a-set)) true
          :else (recur x (rest a-set)))))

(defn ul-adjoin-set[x a-set]
  (if (ul-element-of-set? x a-set)
    a-set
    (cons x a-set)))

(defn ul-intersection-set[set1 set2]
  (cond (or (empty? set1) (empty? set2)) '()
        (ul-element-of-set? (first set1) set2) (cons (first set1) (ul-intersection-set (rest set1) set2))
        :else (ul-intersection-set (rest set1) set2)))

(defn ul-make-set[& args]
  args)

