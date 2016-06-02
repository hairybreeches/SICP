(ns sicp.chapter-4.logic.unique
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.frames)
  (:require [schema.core :as s]))


(s/defn find-unique :- Frame-Stream
        [pattern frame :- Frame rule-stack]
  (let [result (qeval pattern (list frame) rule-stack)]
    (if (= (count result) 1)
      result
      '())))

(defmethod qeval-dispatch 'unique [_ query-pattern frames rule-stack]
  (mapcat #(find-unique (first query-pattern) % rule-stack) frames))
