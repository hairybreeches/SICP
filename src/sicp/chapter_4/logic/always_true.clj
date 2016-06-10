(ns sicp.chapter-4.logic.always-true
  (:use sicp.chapter-4.logic.frames)
  (:require [schema.core :as s])
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.rule-stack))

(s/defn always-true
        [frame :- Frame
         rule-stack :- Rule-Stack
         succeed
         fail]
  (succeed frame fail))

(defmethod analyse-dispatch 'always-true
  [_ query-pattern]
  always-true)
