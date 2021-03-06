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

;sets as orderered lists
(defn ol-element-of-set?[x a-set]
  (loop [x x
         a-set a-set]
    (cond (empty? a-set) false
          (= x (first a-set)) true
          (< x (first a-set)) false
          :else (recur x (rest a-set)))))

;note that this isn't really "half" the number of steps of the unordered version,
;since in the unordered version the append just involves slamming the new element on the front. so the worst case is traversing the list twice.
;it's still order n, but you need to parse the "before" elements three times (once on the stack, once for the reverse and once for the concat)
;the version on the solutions wiki (http://community.schemewiki.org/?sicp-ex-2.61) only goes through the "before" stack once
;since you avoid the reverse + concat by queuing the appends up as part of one "step".
;I'm pretty sure it's a bit more efficient in terms of operations
;(although obviously no tail recursion makes it a moot point since large sets will crash the stack)
;but the slight complication about what precisely constitutes a "step" kinda shows why big-O notation is a useful tool.
(defn ol-adjoin-set[x a-set]
  (loop [before '()
         after a-set]
    (let [current (first after)]
      (cond (= x current) a-set
            (> x current) (recur (cons current before) (rest after))
            (< x current) (concat (reverse (cons x before)) after)))))

(defn ol-intersection-set[set1 set2]
  (if (or (empty? set1) (empty? set2))
      '()
      (let [x1 (first set1)
            x2 (first set2)]
          (cond (= x1 x2) (cons x1 (ol-intersection-set (rest set1) (rest set2)))
                (< x1 x2) (ol-intersection-set (rest set1) set2)
                (> x1 x2) (ol-intersection-set set1 (rest set2))))))

(defn ol-union-set[set1 set2]
  (reverse
    (loop [set1 set1
           set2 set2
           result '()]
      (cond (empty? set1) (concat (reverse set2) result)
            (empty? set2) (concat (reverse set1) result)
            :else (let [x1 (first set1)
                        x2 (first set2)]
                    (cond (= x1 x2) (recur (rest set1) (rest set2) (cons x1 result))
                          (< x1 x2) (recur (rest set1) set2 (cons x1 result))
                          (> x1 x2) (recur set1 (rest set2) (cons x2 result))))))))

(defn ol-make-set[& args]
  (sort args))

(defn ol-set=[set1 set2]
  (= set1 set2))

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

;trees
(defn entry [tree]
  (first tree))

(defn left-branch [tree]
  (second tree))

(defn right-branch [tree]
  (nth tree 2))

(defn make-tree [entry left right]
  (list entry left right))

(defn tree->list-1 [tree]
  (if (empty? tree)
      '()
    (concat (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(defn tree->list-2 [tree]
  (defn copy-to-list [tree result-list]
    (if (empty? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(defn partial-tree[elements n]
  (if (= n 0)
      (cons '() elements)
      (let [left-size (quot (dec n) 2)
            left-result (partial-tree elements left-size)
            left-tree (first left-result)
            non-left-elements (rest left-result)
            right-size (- n (inc left-size))
            this-entry (first non-left-elements)
            right-result (partial-tree (rest non-left-elements) right-size)
            right-tree (first right-result)
            remaining-elements (rest right-result)]
        (cons (make-tree this-entry left-tree right-tree) remaining-elements))))

(defn list->tree[elements]
  (first (partial-tree elements (count elements))))

;sets as trees
(defn tree-element-of-set?[x a-set]
  (cond (empty? a-set) false
        (= x (entry a-set)) true
        (< x (entry a-set)) (tree-element-of-set? x (left-branch a-set))
        (> x (entry a-set)) (tree-element-of-set? x (right-branch a-set))))

(defn tree-adjoin-set[x a-set]
  (if (empty? a-set)
      (make-tree x '() ())
      (let [current (entry a-set)]
        (cond
          (= x current) a-set
          (< x current) (make-tree current (tree-adjoin-set x (left-branch a-set)) (right-branch a-set))
          (> x current) (make-tree current (left-branch a-set) (tree-adjoin-set x (right-branch a-set)))))))

(defn tree-intersection-set[set1 set2]
  (list->tree (ol-intersection-set (tree->list-1 set1) (tree->list-1 set2))))

(defn tree-union-set[set1 set2]
  (list->tree (ol-union-set (tree->list-1 set1) (tree->list-1 set2))))

(defn tree-make-set[& args]
  (list->tree (sort args)))

(defn tree-set=[set1 set2]
  (= (tree->list-2 set1) (tree->list-2 set2)))

;lookup from numerical tree

(defn make-kvp[a-key a-value]
  (list a-key a-value))

(defn get-key[kvp]
  (first kvp))

(defn get-value[kvp]
  (second kvp))

(defn lookup [a-key tree]
  (if (empty? tree) (throw (Exception. "key not found"))
        (let [current-key (get-key (entry tree))]
          (cond
            (= a-key current-key) (get-value (entry tree))
            (< a-key current-key) (lookup a-key (left-branch tree))
            (> a-key current-key) (lookup a-key (right-branch tree))))))









