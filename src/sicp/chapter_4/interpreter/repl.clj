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
  (:use sicp.chapter-4.interpreter.boolean-operators)
  (:use sicp.chapter-4.interpreter.unless))

(def input-prompt ";;; Amb-Eval input:")
(def output-prompt ";;; Amb-Eval value:")
(def new-problem ";;; Starting a new problem")
(def end-problem ";;; There are no more values of:")
(def no-problem ";;; There is no current problem")

(defn- prompt-for-input
  []
  (prn)
  (prn)
  (prn input-prompt))

(defn- announce-output
  []
  (prn)
  (prn output-prompt))

(defn- announce-new-problem
  []
  (prn)
  (prn new-problem))

(defn- announce-end
  [input]
  (prn)
  (prn end-problem)
  (prn input))

(defn- announce-no-problem
  []
  (prn)
  (prn no-problem))

(defn- my-eval [exp env succeed fail]
  ((analyse exp) env succeed fail))

(defn execute
  [exp] (my-eval exp (create-new-environment)))

(defn driver-loop
  []
  (let [global-env (create-new-environment)]
    (loop []
      (let [internal-loop
        (fn [try-again]
          (prompt-for-input)
          (let [input (read)]
            (if
              (= input 'try-again)
              (try-again)
              (do
                (announce-new-problem)
                (my-eval
                  input
                  global-env
                  (fn [value next-alternative]
                    (announce-output)
                    (prn value)
                    (internal-loop next-alternative global-env))
                  (fn []
                    (announce-end input)
                    (recur)))))))]
        (internal-loop
          (fn []
            (announce-no-problem)
            (recur)))))))

