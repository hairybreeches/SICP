(ns sicp.chapter-4.logic.frames
  (:use sicp.chapter-4.logic.query-syntax))

(defn binding-value [bind]
  (:value bind))

(defn binding-variable
  [bind]
  (:variable bind))

(defn make-binding
  [variable value]
  {:variable variable :value value})

(defn binding-in-frame [k frame]
  (frame k))

(defn instantiate [exp frame unbound-var-handler]
  (cond (variable? exp)
        (let [result (binding-in-frame exp frame)]
          (if result
            (instantiate (binding-value result) frame unbound-var-handler)
            (unbound-var-handler exp frame)))

          (seq? exp)
          (map #(instantiate % frame unbound-var-handler) exp)

          :else exp))

(defn extend-frame [variable datum frame]
  (assoc frame variable (make-binding variable datum)))

(defn create-empty-frame []
  {})
