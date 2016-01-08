(ns sicp.chapter-4.definition
  (:use sicp.chapter-4.evaluator)
  (:use sicp.chapter-4.lambda)
  (:use sicp.chapter-4.environments))

(defn- definition-variable
  [exp]
  (if (symbol? (second exp))
      (second exp)
      (first (second exp))))

(defn- definition-value
  [exp]
  (if (symbol? (second exp))
      (nth exp 2)
      (make-lambda
        (rest (second exp))
        (drop 2 exp))))

(defn- eval-definition
  [exp env]
  (define-variable!
    (definition-variable exp)
    (my-eval (definition-value exp) env)
    env)
  :ok)

(defmethod my-eval 'definition [exp env]
  (eval-definition exp env))
