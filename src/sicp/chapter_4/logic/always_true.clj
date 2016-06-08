(ns sicp.chapter-4.logic.always-true
  (:use sicp.chapter-4.logic.frames)
  (:require [schema.core :as s])
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.rule-stack))

(s/defn always-true :- Frame-Stream
        [frames :- Frame-Stream
         rule-stack :- Rule-Stack]
  frames)

(defmethod analyse-dispatch 'always-true
  [_ query-pattern]
  always-true)
