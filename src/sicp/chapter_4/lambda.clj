(ns sicp.chapter-4.lambda
  (:use sicp.chapter-4.procedures)
  (:use sicp.chapter-4.evaluator))

(defn- lambda-parameters
  [exp]
  (second exp))

(defn- lambda-body
  [exp]
  (drop 2 exp))

(defn make-lambda
  [parameters body]
  (cons 'lambda (cons parameters body)))

(defmethod my-eval 'lambda [exp env]
  (make-procedure
    (lambda-parameters exp)
    (lambda-body exp)
    env))
