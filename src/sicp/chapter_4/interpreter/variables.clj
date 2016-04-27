(ns sicp.chapter-4.interpreter.variables
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.environments))

(defmethod my-eval clojure.lang.Symbol [exp env] (lookup-variable-value exp env))
