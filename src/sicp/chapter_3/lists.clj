(ns sicp.chapter-3.lists
  (:use sicp.chapter-2.pairs))

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

