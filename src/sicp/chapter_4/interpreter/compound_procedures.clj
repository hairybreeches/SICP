(ns sicp.chapter-4.interpreter.compound-procedures
  (:use sicp.chapter-4.interpreter.variable-hoisting)
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.lambda)
  (:use sicp.chapter-4.interpreter.environments)
  (:use sicp.chapter-4.interpreter.laziness)
  (:use sicp.chapter-4.interpreter.begin))

(defn- make-procedure
  [parameters action-list env parameter-resolution]
  (list 'procedure parameters (hoist-variables action-list) env parameter-resolution))

(defn procedure-body
  [p]
  (nth p 2))

(defn procedure-parameter-names
  [p]
  (second p))

(defn- procedure-environment
  [p]
  (nth p 3))

(defn- procedure-parameter-resolution
  [p]
  (nth p 4))

(defn- get-parameter-name
  [p]
  (if (seq? p)
      (first p)
      p))


(defn- lookup-parameter-resolution
  [p]
  (if (seq? p)
      (cond
        (= (second p) 'lazy-memo) delay-it
        (= (second p) 'lazy) delay-it-no-memo)
      actual-value))

(defn- list-of-args
  [exps env resolution-functions]
  (if (empty? exps)
    '()
    (cons ((first resolution-functions) (first exps) env)
          (list-of-args (rest exps) env (rest resolution-functions)))))

(defmethod my-apply 'procedure
  [procedure arguments env]
  (my-eval
    (procedure-body procedure)
    (extend-environment (procedure-parameter-names procedure)
                        (list-of-args arguments env (procedure-parameter-resolution procedure))
                        (procedure-environment procedure))))

(defmethod my-eval 'lambda [exp env]
  (let [parameters (lambda-parameters exp)]
  (make-procedure
    (map get-parameter-name parameters)
    (lambda-body exp)
    env
    (map lookup-parameter-resolution parameters))))
