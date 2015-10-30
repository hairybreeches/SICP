(ns sicp.chapter-3.lists
  (:use sicp.chapter-2.pairs))

(defn make-list
  [length]
    (if (= length 0)
        nil
        (cons-pair length (make-list (dec length)))))

(defn find-tail
  [pair-list]
  (loop [pair-list pair-list]
    (if (nil? (cdr pair-list))
        pair-list
        (recur (cdr pair-list)))))

(defn make-cycle
  [length]
  (let [head (make-list length)
        tail (find-tail head)]
    (set-cdr! tail head)
    head))

(defn make-cycle-with-tail
  [cycle-length tail-length]
   (let [pair-cycle (make-cycle cycle-length)
         head (make-list tail-length)]
     (set-cdr! (find-tail head) pair-cycle)
     head))

(defn naive-count-pairs
  [x]
  (if (not (pair? x))
      0
    (+ (naive-count-pairs (car x))
       (naive-count-pairs (cdr x))
       1)))

(defn count-pairs
  [x]
  (defn count-pairs-inner
    [object already-counted]
    (if (or (not (pair? object))
            (already-counted object))
        {:count 0 :counted already-counted}
        (let [{left-count :count left-counted :counted} (count-pairs-inner (car object) (conj already-counted object))
              {right-count :count all-counted :counted} (count-pairs-inner (cdr object) left-counted)]
              {:count (+ 1 left-count right-count) :counted all-counted})))
  (:count (count-pairs-inner x #{})))

;there is a solution which doesn't butcher the structure, but not too bad!

;the tortoise and the hare algorithm holds two pointers,
;one "a" which moves along one per step and another "b" which moves along two per step.

;obviously if at any stage we run out of elements, there is no cycle.

;if at any stage "a" and "b" are equal then we clearly have a cycle ("b" has caught up with "a")

;the clever part is that because "b" catches up on "a" one item every iteration once they enter a cycle,
;no matter where "b" is when "a" first enters the cycle, sooner or later "b" will catch and land on "a"
(defn contains-cycle?
  [x]
  (if (not (pair? x))
      false
      (loop [pair x]
        (let [nxt (cdr pair)]
          (cond
            (not (pair? nxt)) false
            (= nxt pair) true
            :else (do (set-cdr! pair (cdr nxt))
                      (recur nxt)))))))

