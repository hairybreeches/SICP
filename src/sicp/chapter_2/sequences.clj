(ns sicp.chapter-2.sequences
  (:use sicp.chapter-2.pairs))

(defn last-pair[things]
  (loop [things things]
    (let [cdr (rest things)]
      (if (empty? cdr)
        (first things)
        (recur cdr)))))

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

(defn same-parity [indicator & values]
  (let [remainder (rem indicator 2)]
    (reverse
       (loop [values (cons indicator values)
            result '()]
         (cond
            (empty? values) result

            (= (rem (first values) 2) remainder)
              (recur
                (rest values)
                (cons (first values)  result))

        :else (recur (rest values) result))))))

(defn square[x]
  (*' x x))

(defn square-list-primitives[items]
  (if (empty? items)
    '()
    (cons (square (first items)) (square-list-primitives (rest items)))))

(defn square-list-map[items]
  (map square items))

(defn for-each[function items]
  (loop [items items]
    (if (empty? items)
      true
      (do
        (function (first items))
        (recur (rest items))))))

(defn deep-reverse[things]
    (cond (not (coll? things)) things
          (empty? things) things
          :else (concat (deep-reverse (rest things)) [(deep-reverse (first things))])))

(defn fringe[things]
    (cond (not (coll? things)) [things]
          (empty? things) []
          :else (concat (fringe (first things)) (fringe (rest things)))))

;only the selectors and constructors need to change if we change the representation of mobiles
;(weight? would need to be changed if we stopped representing weight structures as a simple integer.
(defn make-mobile [left right]
  (func-cons left right))

(defn make-branch [length structure]
  (func-cons length structure))

(defn left-branch[mobile]
  (func-car mobile))

(defn right-branch[mobile]
  (func-cdr mobile))

(defn branch-length[branch]
  (func-car branch))

(defn branch-structure[branch]
  (func-cdr branch))

(defn weight? [structure]
  (number? structure))

(defn total-weight[mobile]
  (if (weight? mobile)
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(defn torque[branch]
  (* (branch-length branch) (total-weight (branch-structure branch))))

(defn simply-balanced? [mobile]
  (= (torque (left-branch mobile)) (torque (right-branch mobile))))

(defn balanced?[mobile]
  (if (weight? mobile) true
      (and (simply-balanced? mobile)
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

(defn square-tree-primitive[tree]
  (cond (not (coll? tree)) (square tree)
        (empty? tree) tree
        :else (cons (square-tree-primitive (first tree)) (square-tree-primitive (rest tree)))))

(defn tree-map[function tree]
  (map #(if (not (coll? %)) (function %) (tree-map function %)) tree))

(defn square-tree-map[tree]
  (tree-map square tree))

(defn subsets[elements]
  (if (empty? elements) '(())
      (let [current (first elements)
            subsets-without-current (subsets (rest elements))]
        (concat subsets-without-current (map #(cons current %) subsets-without-current)))))

(defn fold-right[op initial items]
  (if (empty? items) initial
      (op (first items) (fold-right op initial (rest items)))))

;note that this is equivalent to Clojure's map
(defn fold-left[op initial items]
  (loop [result initial
         items items]
    (if (empty? items) result
      (recur (op result (first items))
             (rest items)))))

(defn map-impl [proc items]
  (fold-right (fn [current so-far] (cons (proc current) so-far)) '() items))

(defn append [seq1 seq2]
  (fold-right cons seq2 seq1))

(defn length [items]
  (fold-right (fn [current so-far] (inc so-far)) 0 items))

(defn horner-eval[x coefficients]
  (fold-right (fn [this-coefficient higher-terms] (+ this-coefficient (* higher-terms x))) 0 coefficients))

(defn count-leaves[tree]
  (fold-right + 0 (map #(if (seq? %) (count-leaves %) 1) tree)))

(defn fold-right-n [op init seqs]
  (if (empty? (first seqs)) '()
      (cons (fold-right op init (map first seqs))
            (fold-right-n op init (map rest seqs)))))

(defn dot-product[v w]
  (fold-right + 0 (map * v w)))

(defn matrix-*-vector[m v]
  (map #(dot-product % v) m))

(defn transpose[m]
  (fold-right-n cons '() m))

(defn matrix-*[m n]
  (let [cols (transpose n)]
    (map #(matrix-*-vector cols %) m)))

(defn reverse-foldr[items]
  (fold-right (fn[current so-far] (concat so-far [current])) '() items))

(defn reverse-foldl[items]
  (fold-left (fn[so-far current] (cons current so-far)) '() items))














