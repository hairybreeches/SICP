(ns sicp.chapter-4.logic.always-true
  (:use sicp.chapter-4.logic.frames)
  (:require [schema.core :as s])
  (:use sicp.chapter-4.logic.evaluation))

(s/defn always-true
        [ignore
         frames :- Frame-Stream]
  frames)

(defmethod qeval-dispatch 'always-true
  [_ query-pattern frames rule-stack]
  (always-true query-pattern frames))
