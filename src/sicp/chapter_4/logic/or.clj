(ns sicp.chapter-4.logic.or
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.sequences)
  (:use sicp.chapter-4.logic.frames)
  (:require [schema.core :as s]))

(s/defn disjoin :- Frame-Stream
  [disjuncts frames :- Frame-Stream rule-stack]
  (if (empty? disjuncts)
    '()
    (interleave-all
      (qeval (first disjuncts) frames rule-stack)
      (disjoin (rest disjuncts) frames rule-stack))))

(defmethod qeval-dispatch 'or [_ query-pattern frames rule-stack]
  (disjoin query-pattern frames rule-stack))
