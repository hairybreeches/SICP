(ns sicp.chapter-4.logic.frames
  (:use sicp.chapter-4.logic.query-syntax)
  (:require [schema.core :as s]))


(def Binding
  "schema for a variable binding"
  {:value s/Any :variable s/Any})

(def Frame
  "schema for a frame"
  {s/Any Binding})

(def Frame-Stream
  "schema for a sequnce of frames"
  [Frame])

(s/defn binding-value
        [bind :- Binding]
  (:value bind))

(s/defn binding-variable
  [bind :- Binding]
  (:variable bind))

(s/defn make-binding :- Binding
  [variable value]
  {:variable variable :value value})

(s/defn binding-in-frame
  [k frame :- Frame]
  (frame k))

(s/defn instantiate [exp frame :- Frame unbound-var-handler]
  (cond (variable? exp)
        (let [result (binding-in-frame exp frame)]
          (if result
            (instantiate (binding-value result) frame unbound-var-handler)
            (unbound-var-handler exp frame)))

          (seq? exp)
          (map #(instantiate % frame unbound-var-handler) exp)

          :else exp))

(s/defn extend-frame :- Frame
  [variable datum frame :- Frame]
    (assoc frame variable (make-binding variable datum)))

(s/defn create-empty-frame :- Frame
  []
  {})
