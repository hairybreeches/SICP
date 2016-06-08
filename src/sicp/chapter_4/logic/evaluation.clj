(ns sicp.chapter-4.logic.evaluation
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.rule-stack)
  (:require [schema.core :as s]))


(defmulti analyse-dispatch (fn [query-type query] query-type))

(s/defn analyse
  ([query]
   (analyse-dispatch
    (query-type query)
    (query-content query))))

(s/defn prettify
  [query
   frame :- Frame]
  (remove-dots
    (instantiate query frame (fn [v f] (contract-question-mark v)))))

