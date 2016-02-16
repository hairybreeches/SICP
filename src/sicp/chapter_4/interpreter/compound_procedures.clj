(ns sicp.chapter-4.interpreter.compound-procedures
  (:use sicp.chapter-4.interpreter.variable-hoisting)
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.lambda)
  (:use sicp.chapter-4.interpreter.environments)
  (:use sicp.chapter-4.interpreter.laziness)
  (:use sicp.chapter-4.interpreter.begin))

(defn- tagged-list?
  [exp tag]
  (if (seq? exp)
      (= (first exp) tag)
      false))

(defn compound-procedure?
  [p]
  (tagged-list? p 'procedure))

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

(defn list-of-delayed-args
  [exps env]
  (if (empty? exps)
    '()
    (cons (delay-it (first exps) env)
          (list-of-delayed-args (rest exps) env))))

(defmethod my-apply 'procedure
  [procedure arguments env]
  (my-eval
    (procedure-body procedure)
    (extend-environment (procedure-parameters procedure)
                        (list-of-delayed-args arguments env)
                        (procedure-environment procedure))))

(defmethod my-eval 'lambda [exp env]
  (make-procedure
    (lambda-parameters exp)
    (lambda-body exp)
    env))
