(ns sicp.chapter-4.compound-procedures
  (:use sicp.chapter-4.evaluator)
  (:use sicp.chapter-4.environments)
  (:use sicp.chapter-4.lambda))

(defn- tagged-list?
  [exp tag]
  (if (seq? exp)
      (= (first exp) tag)
      false))

(defn compound-procedure?
  [p]
  (tagged-list? p 'procedure))

(defn make-procedure
  [parameters body env]
  (list 'procedure parameters body env))

(defn procedure-body
  [p]
  (nth p 2))

(defn procedure-parameters
  [p]
  (second p))

(defn procedure-environment
  [p]
  (nth p 3))

(defmethod my-apply 'procedure
  [procedure arguments]
  (eval-sequence
    (procedure-body procedure)
    (extend-environment (procedure-parameters procedure)
                        arguments
                        (procedure-environment procedure))))

(defmethod my-eval 'lambda [exp env]
  (make-procedure
    (lambda-parameters exp)
    (lambda-body exp)
    env))
