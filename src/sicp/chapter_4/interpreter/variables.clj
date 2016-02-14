(ns sicp.chapter-4.interpreter.variables
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.environments))

(defmethod analyse clojure.lang.Symbol [exp]
  (fn [env] (lookup-variable-value exp env)))
