(ns sicp.chapter-4.logic.rule-stack
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.frames)
  (:require [schema.core :as s]))

(def Rule-Stack-Layer
  {:rule s/Any
   :rule-values s/Any})

(def Rule-Stack
  [Rule-Stack-Layer])

(s/defn make-stack-layer :- Rule-Stack-Layer
  [rule-instance rule-general frame :- Frame]
  {:rule rule-general
   :rule-values (instantiate (conclusion rule-instance) frame (fn [v f] '?))})

(s/defn duplicate-stack-layer?
  [rule-stack :- Rule-Stack
   stack-layer :- Rule-Stack-Layer]
  (some #{stack-layer} rule-stack))
