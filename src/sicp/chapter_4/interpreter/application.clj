(ns sicp.chapter-4.interpreter.application
  (:use sicp.chapter-4.interpreter.evaluator))

(defmethod analyse :default [exp]
  (let [fproc (analyse (operator exp))
        aprocs (map analyse (operands exp))]
    (fn [env]
      (execute-application (fproc env) (map #(% env) aprocs)))))
