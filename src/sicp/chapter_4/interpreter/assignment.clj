(ns sicp.chapter-4.interpreter.assignment
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.environments))


(defn- assignment-variable
  [exp]
  (first (operands exp)))

(defn- assignment-value
  [exp]
  (second (operands exp)))

(defn- analyse-assignment
  [exp]
  (let [var-name (assignment-variable exp)
        value-proc (analyse (assignment-value exp))]
    (fn [env]
      (set-variable-value! var-name (value-proc env) env)
      :ok)))

(defmethod analyse 'set! [exp]
  (analyse-assignment exp))

(defn make-set
  [var-name var-value]
  (create-expression 'set! (list var-name var-value)))
