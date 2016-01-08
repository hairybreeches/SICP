(ns sicp.chapter-4.variables
  (:use sicp.chapter-4.evaluator)
  (:use sicp.chapter-4.environments))

(defmethod my-eval clojure.lang.Symbol [exp env] (lookup-variable-value exp env))
