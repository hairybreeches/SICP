(ns sicp.chapter-4.interpreter.repl
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.if)
  (:use sicp.chapter-4.interpreter.cond)
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
  (:use sicp.chapter-4.interpreter.default-environment)
  (:use sicp.chapter-4.interpreter.boolean-operators)
  (:use sicp.chapter-4.interpreter.unless)
  (:use sicp.chapter-4.interpreter.laziness)
  (:use sicp.chapter-4.interpreter.lazy-lists))

(def input-prompt ";;; L-Eval input:")
(def output-prompt ";;; L-Eval value:")

(defn- prompt-for-input
  []
  (prn)
  (prn)
  (prn input-prompt))

(defn- announce-output
  []
  (prn)
  (prn output-prompt))


(defn execute
  [& expressions]
  (actual-value
    (sequence->exp expressions)
    (create-new-environment)))

(defn driver-loop
  []
  (let [global-env (create-new-environment)]
  (loop []
    (prompt-for-input)
      (let [input (read)
            output (actual-value input global-env)]
        (announce-output)
        (prn output))
    (recur))))

