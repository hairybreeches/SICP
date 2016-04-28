(ns sicp.chapter-4.logic.or
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.sequences))

(defn- disjoin [disjuncts frames]
  (if (empty? disjuncts)
    '()
    (interleave-all
      (qeval (first disjuncts) frames)
      (disjoin (rest disjuncts) frames))))

(defmethod qeval-dispatch 'or [_ query-pattern frames]
  (disjoin query-pattern frames))
