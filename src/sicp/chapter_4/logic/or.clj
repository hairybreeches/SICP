(ns sicp.chapter-4.logic.and
  (:use sicp.chapter-4.logic.evaluation))

(defn- disjoin [disjuncts frames]
  (if (empty? disjuncts)
    '()
    (interleave
      (qeval (first disjuncts) frames)
      (disjoin (rest disjuncts) frames))))

(defmethod qeval :or [query-pattern frames]
  (disjoin query-pattern frames))
