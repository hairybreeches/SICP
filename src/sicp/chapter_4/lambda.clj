(ns sicp.chapter-4.lambda
  (:use sicp.chapter-4.evaluator))

(defn lambda-parameters
  [exp]
  (first (operands exp)))

(defn lambda-body
  [exp]
  (rest (operands exp)))

(defn make-lambda
  [parameters body]
  (create-expression 'lambda (cons parameters body)))
