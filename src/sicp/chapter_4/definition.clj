(ns sicp.chapter-4.definition
  (:use sicp.chapter-4.evaluator))

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

(def define-variable!)

(defn- eval-definition
  [exp env]
  (define-variable!
    (definition-variable exp)
    (my-eval (definition-value exp) env)
    env)
  :ok)

(defmethod eval-list-expression 'definition [exp env]
  (eval-definition exp env))
