(ns sicp.chapter-4.logic.simple-query
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.assertions)
  (:use sicp.chapter-4.logic.rules)
  (:use sicp.chapter-4.logic.rule-stack)
  (:require [schema.core :as s]))

(s/defn evaluate-simple-query
        [query-pattern
         frames :- Frame-Stream
         rule-stack :- Rule-Stack]
  (mapcat
    #(concat
       (find-assertions query-pattern %)
       (apply-rules query-pattern % rule-stack))
    frames))

(defmethod qeval-dispatch :default [query-type query-pattern frames rule-stack]
  (evaluate-simple-query (cons query-type query-pattern) frames rule-stack))
