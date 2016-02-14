(ns sicp.chapter-4.interpreter.definition
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.lambda)
  (:use sicp.chapter-4.interpreter.environments))


(defn- function-definition?
  [exp]
  (symbol? (first (operands exp))))

(defn definition-variable
  [exp]
  (if (function-definition? exp)
      (first (operands exp))
      (first (first (operands exp)))))

(defn definition-value
  [exp]
  (if (function-definition? exp)
      (second (operands exp))
      (make-lambda
        (rest (first (operands exp)))
        (drop 1 (operands exp)))))

(defn- eval-definition
  [exp env]
  (define-variable!
    (definition-variable exp)
    (my-eval (definition-value exp) env)
    env)
  :ok)

(defmethod my-eval 'define [exp env]
  (eval-definition exp env))

(defn- eval-unbind
  [exp env]
  (make-unbound! (first (operands exp)) env))

(defn make-define
  [n v]
  (create-expression 'define (list n v)))

(defmethod my-eval 'unbind! [exp env]
  (eval-unbind exp env))
