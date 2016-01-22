(ns sicp.chapter-4.assignment
  (:use sicp.chapter-4.evaluator)
  (:use sicp.chapter-4.environments))


(defn- assignment-variable
  [exp]
  (first (operands exp)))

(defn- assignment-value
  [exp]
  (second (operands exp)))

(defn- eval-assignment
  [exp env]
  (set-variable-value!
    (assignment-variable exp)
    (my-eval (assignment-value exp) env)
    env)
  :ok)

(defmethod my-eval 'set! [exp env]
  (eval-assignment exp env))
