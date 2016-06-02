(ns sicp.chapter-4.logic.and
  (:use sicp.chapter-4.logic.frames)
  (:require [schema.core :as s])
  (:use sicp.chapter-4.logic.evaluation))

(s/defn conjoin [conjuncts frames :- Frame-Stream rule-stack]
  (loop [conjuncts conjuncts
         frames frames]
    (if (empty? conjuncts)
        frames
      (recur (rest conjuncts)
             (qeval (first conjuncts)
                    frames
                    rule-stack)))))

(defmethod qeval-dispatch 'and [_ query-pattern frames rule-stack]
  (conjoin query-pattern frames rule-stack))

