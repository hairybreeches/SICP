(ns sicp.chapter-4.logic.or
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.sequences))

(defn- disjoin [disjuncts frames rule-stack]
  (if (empty? disjuncts)
    '()
    (interleave-all
      (qeval (first disjuncts) frames rule-stack)
      (disjoin (rest disjuncts) frames rule-stack))))

(defmethod qeval-dispatch 'or [_ query-pattern frames rule-stack]
  (disjoin query-pattern frames rule-stack))
