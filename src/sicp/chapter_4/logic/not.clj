(ns sicp.chapter-4.logic.not
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.rule-stack)
  (:require [schema.core :as s]))

(s/defn analyse-not
        [query-to-negate]
        (let [analysed-query (analyse query-to-negate)]
          (s/fn
            [frame :- Frame
             rule-stack :- Rule-Stack
             success
             fail]
            (analysed-query
              frame
              rule-stack
              (s/fn
                [frame :- Frame
                 rule-stack :- Rule-Stack
                 fail2]
                (fail))

              (fn []
                (success frame rule-stack fail))))))


(defmethod analyse-dispatch 'not [_ query-pattern]
  (analyse-not (negated-query query-pattern)))
