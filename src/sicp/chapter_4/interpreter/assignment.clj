(ns sicp.chapter-4.interpreter.assignment
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.environments))


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

(defn make-set
  [var-name var-value]
  (create-expression 'set! (list var-name var-value)))
