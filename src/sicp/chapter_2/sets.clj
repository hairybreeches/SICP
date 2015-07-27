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

(defn ul-union-set[set1 set2]
  (loop [set1 set1
         set2 set2]
    (cond (empty? set1) set2
          :else (recur (rest set1) (ul-adjoin-set (first set1) set2)))))

(defn ul-make-set[& args]
  args)

(defn ul-set=[set1 set2]
  (= (sort set1) (sort set2)))

;sets as unorderered lists with repeats
(defn ul-repeat-element-of-set?[x a-set]
  (loop [x x
         a-set a-set]
    (cond (empty? a-set) false
          (= x (first a-set)) true
          :else (recur x (rest a-set)))))

(defn ul-repeat-adjoin-set[x a-set]
    (cons x a-set))

(defn ul-repeat-intersection-set[set1 set2]
  (cond (or (empty? set1) (empty? set2)) '()
        (ul-element-of-set? (first set1) set2) (cons (first set1) (ul-intersection-set (rest set1) set2))
        :else (ul-intersection-set (rest set1) set2)))

(defn ul-repeat-union-set[set1 set2]
  (concat set1 set2))

(defn ul-repeat-make-set[& args]
  args)

(defn ul-repeat-set=[set1 set2]
  (= (sort (distinct set1)) (sort (distinct set2))))
