(ns sicp.chapter-4.interpreter.compound-procedures
  (:use sicp.chapter-4.interpreter.variable-hoisting)
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.lambda)
  (:use sicp.chapter-4.interpreter.environments))

(defn- tagged-list?
  [exp tag]
  (if (seq? exp)
      (= (first exp) tag)
      false))

(defn compound-procedure?
  [p]
  (tagged-list? p 'procedure))

(defn- make-procedure
  [parameters body-proc env]
  (list 'procedure parameters body-proc env))

(defn procedure-body
  [p]
  (nth p 2))

(defn procedure-parameters
  [p]
  (second p))

(defn- procedure-environment
  [p]
  (nth p 3))

(defmethod execute-application 'procedure
  [procedure arguments]
  ((procedure-body procedure)
    (extend-environment (procedure-parameters procedure)
                        arguments
                        (procedure-environment procedure))))

(defmethod analyse 'lambda [exp]
  (let [vars (lambda-parameters exp)
        body (analyse (hoist-variables (sequence->exp (lambda-body exp))))]
    (fn [env]
      (make-procedure vars body env))))
