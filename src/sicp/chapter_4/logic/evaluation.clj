(ns sicp.chapter-4.logic.evaluation
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.frames)
  (:require [schema.core :as s]))


(defmulti qeval-dispatch (fn [query-type query frames rule-stack] query-type))

(s/defn qeval :- Frame-Stream
  ([query frames :- Frame-Stream]
   (qeval query frames '()))

  ([query frames :- Frame-Stream rule-stack]

   (qeval-dispatch
    (query-type query)
    (query-content query)
    frames
     rule-stack)))

(defn prettify
  [query frame]
  (remove-dots
    (instantiate query frame (fn [v f] (contract-question-mark v)))))

