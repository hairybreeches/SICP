(ns sicp.chapter-4.logic.unique
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.rule-stack)
  (:require [schema.core :as s]))


(s/defn find-unique :- Frame-Stream
        [fun
         frame :- Frame
         rule-stack :- Rule-Stack]

  (let [result (fun (list frame) rule-stack)]
    (if (= (count result) 1)
      result
      '())))

(s/defn analyse-unique [unique-statement]
        (let [analysed-statement (analyse unique-statement)]
        (s/fn :- Frame-Stream
          [frames :- Frame-Stream
           rule-stack :- Rule-Stack]
              (mapcat #(find-unique analysed-statement % rule-stack) frames))))

(defmethod analyse-dispatch 'unique [_ query-pattern]
  (analyse-unique (first query-pattern)))
