(ns sicp.chapter-4.logic.rule-stack
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.frames)
  (:require [schema.core :as s]))

(s/defn make-stack-layer
  [rule-instance rule-general frame :- Frame]
  {:rule rule-general
   :rule-values (instantiate (conclusion rule-instance) frame (fn [v f] '?))})

(defn duplicate-stack-layer? [rule-stack stack-layer]
  (some #{stack-layer} rule-stack))
