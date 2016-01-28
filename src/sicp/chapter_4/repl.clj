(ns sicp.chapter-4.repl
  (:use sicp.chapter-4.evaluator)
  (:use sicp.chapter-4.if)
  (:use sicp.chapter-4.cond)
  (:use sicp.chapter-4.begin)
  (:use sicp.chapter-4.assignment)
  (:use sicp.chapter-4.lambda)
  (:use sicp.chapter-4.quote)
  (:use sicp.chapter-4.definition)
  (:use sicp.chapter-4.application)
  (:use sicp.chapter-4.primitive-datatypes)
  (:use sicp.chapter-4.procedures)
  (:use sicp.chapter-4.variables)
  (:use sicp.chapter-4.let)
  (:use sicp.chapter-4.while)
  (:use sicp.chapter-4.boolean-operators))

(def input-prompt ";;; M-Eval input:")
(def output-prompt ";;; M-Eval value:")

(defn- prompt-for-input
  [string]
  (prn)
  (prn)
  (prn string)
  (prn))

(defn- announce-output
  [string]
  (prn)
  (prn string)
  (prn))

(defn- user-print
  [object]
  (if (compound-procedure? object)
      (prn `(
              compound-procedure
              ~(procedure-parameters object)
              ~(procedure-body object)
              <procedure-env>))
      (prn object)))

(defn driver-loop
  []
  (loop []
    (prompt-for-input input-prompt)
      (let [input (read)
            output (execute input)]
        (announce-output output-prompt)
        (user-print output))
    (recur)))

