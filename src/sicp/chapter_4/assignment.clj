(ns sicp.chapter-4.assignment
  (:use sicp.chapter-4.evaluator))


(defn- assignment-variable
  [exp]
  (second exp))

(defn- assignment-value
  [exp]
  (nth exp 2))

(def set-variable-value!)

(defn- eval-assignment
  [exp env]
  (set-variable-value!
    (assignment-variable exp)
    (my-eval (assignment-value exp) env)
    env)
  :ok)

(defmethod eval-list-expression 'set! [exp env]
  (eval-assignment exp env))
