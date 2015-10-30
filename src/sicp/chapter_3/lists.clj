(ns sicp.chapter-3.lists
  (:use sicp.chapter-2.pairs))

(defn naive-count-pairs
  [x]
  (if (not (pair? x))
      0
    (+ (naive-count-pairs (car x))
       (naive-count-pairs (cdr x))
       1)))
