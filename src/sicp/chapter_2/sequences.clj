(ns sicp.chapter-2.sequences)

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

(defn count-leaves[x]
  (cond (not (or (coll? x) (nil? x))) 1
        (empty? x) 0
        :else (+ (count-leaves (first x))
                 (count-leaves (rest x)))))

(defn deep-reverse[things]
    (cond (not (coll? things)) things
          (empty? things) things
          :else (concat (deep-reverse (rest things)) [(deep-reverse (first things))])))

(defn fringe[things]
    (cond (not (coll? things)) [things]
          (empty? things) []
          :else (concat (fringe (first things)) (fringe (rest things)))))

(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(defn left-branch[mobile]
  (first mobile))

(defn right-branch[mobile]
  (second mobile))

(defn branch-length[branch]
  (first branch))

(defn branch-structure[branch]
  (second branch))

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









