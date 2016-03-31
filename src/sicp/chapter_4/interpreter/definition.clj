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

(defn- analyse-definition
  [exp]
  (let [variable-name (definition-variable exp)
        value-proc (analyse (definition-value exp))]
    (fn [env succeed fail]
      (value-proc
        env
        (fn [value fail2]
            (define-variable! variable-name value env)
            (succeed :ok fail2))
        fail))))

(defmethod analyse 'define [exp]
  (analyse-definition exp))

(defn make-define
  [n v]
  (create-expression 'define (list n v)))

(defn- analyse-unbind
  [exp]
  (let [variable-name (first (operands exp))]
    (fn [env]
      (make-unbound! variable-name env))))

(defmethod analyse 'unbind! [exp]
  (analyse-unbind exp))
