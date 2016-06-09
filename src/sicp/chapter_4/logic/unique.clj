(ns sicp.chapter-4.logic.unique
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.error)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.rule-stack)
  (:require [schema.core :as s]))


(s/defn analyse-unique [unique-statement]
        (error "not yet implemented unique"))

(defmethod analyse-dispatch 'unique [_ query-pattern]
  (analyse-unique (first query-pattern)))
