(ns sicp.chapter-4.interpreter.repl
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.if)
    (:use sicp.chapter-4.interpreter.begin)
  (:use sicp.chapter-4.interpreter.assignment)
  (:use sicp.chapter-4.interpreter.lambda)
  (:use sicp.chapter-4.interpreter.quote)
  (:use sicp.chapter-4.interpreter.definition)
  (:use sicp.chapter-4.interpreter.application)
  (:use sicp.chapter-4.interpreter.primitive-datatypes)
  (:use sicp.chapter-4.interpreter.compound-procedures)
  (:use sicp.chapter-4.interpreter.primitive-procedures)
  (:use sicp.chapter-4.interpreter.variables)
  (:use sicp.chapter-4.interpreter.let)
  (:use sicp.chapter-4.interpreter.while)
  (:use sicp.chapter-4.interpreter.cond)
  (:use sicp.chapter-4.interpreter.default-environment)
  (:use sicp.chapter-4.interpreter.boolean-operators))

(def input-prompt ";;; M-Eval input:")
(def output-prompt ";;; M-Eval value:")

(defn- prompt-for-input
  [string]
  (prn)
  (prn)
  (prn string))

(defn- announce-output
  [string]
  (prn)
  (prn string))

(defn- user-print
  [object]
  (if (compound-procedure? object)
      (prn `(
              compound-procedure
              ~(procedure-parameters object)
              ~(procedure-body object)
              <procedure-env>))
      (prn object)))

(defn execute
  [exp] (my-eval exp (create-new-environment)))

(defn driver-loop
  []
  (let [global-env (create-new-environment)]
  (loop []
    (prompt-for-input input-prompt)
      (let [input (read)
            output (my-eval input global-env)]
        (announce-output output-prompt)
        (user-print output))
    (recur))))

