(ns sicp.chapter-4.logic.or
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.sequences)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.rule-stack)
  (:require [schema.core :as s]))

(s/defn analyse-or
        [or-statements]
        (let [analysed-statements (map analyse or-statements)]
          (s/fn :- Frame-Stream
          [frames :- Frame-Stream
           rule-stack :- Rule-Stack]
          (->>
            analysed-statements
            (map #(% frames rule-stack))
            (apply interleave-all)))))

(defmethod analyse-dispatch 'or [_ query-pattern]
  (analyse-or query-pattern))
