(ns sicp.chapter-4.interpreter.compound-procedures
  (:use sicp.chapter-4.interpreter.variable-hoisting)
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.lambda)
  (:use sicp.chapter-4.interpreter.environments))

(defn- make-procedure
  [parameters action-list env]
  (list 'procedure parameters (hoist-variables action-list) env))

(defn procedure-body
  [p]
  (nth p 2))

(defn procedure-parameters
  [p]
  (second p))

(defn- procedure-environment
  [p]
  (nth p 3))

(defmethod my-apply 'procedure
  [procedure arguments]
  (my-eval
    (procedure-body procedure)
    (extend-environment (procedure-parameters procedure)
                        arguments
                        (procedure-environment procedure))))

(defmethod my-eval 'lambda [exp env]
  (make-procedure
    (lambda-parameters exp)
    (lambda-body exp)
    env))
