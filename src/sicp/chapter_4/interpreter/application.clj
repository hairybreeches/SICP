(ns sicp.chapter-4.interpreter.application
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.laziness))

(defmethod my-eval :default [exp env]
  (my-apply
    (actual-value (operator exp) env)
    (operands exp)
    env))
