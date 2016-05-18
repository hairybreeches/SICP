(ns sicp.chapter-4.logic.frames)

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

(defn exten [variable datum frame]
  (assoc frame variable (make-binding variable datum)))

(defn create-empty-frame []
  {})

(defn duplicate-stack-layer? [rule-stack stack-layer]
  (some #{stack-layer} rule-stack))
