(ns sicp.chapter-4.logic.not
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.rule-stack)
  (:require [schema.core :as s]))

(s/defn analyse-not
  [query-to-negate]
        (let [analysed-query (analyse query-to-negate)]
          (s/fn :- Frame-Stream
                [frames :- Frame-Stream
                 rule-stack :- Rule-Stack]
                (filter
                  (fn [frame]
                    (empty? (analysed-query (list frame) rule-stack)))
                  frames))))


(defmethod analyse-dispatch 'not [_ query-pattern]
  (analyse-not (negated-query query-pattern)))
