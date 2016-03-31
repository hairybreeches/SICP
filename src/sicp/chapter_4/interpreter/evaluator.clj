(ns sicp.chapter-4.interpreter.evaluator
  (:use sicp.error))

;syntax
(defn operator
  [exp]
  (first exp))

(defn operands
  [exp]
  (rest exp))

(defn create-expression
  [form operands]
  (cons form operands))

;dispatch functions
(defn get-exp-type
  [exp]
     (if (seq? exp)
         (operator exp)
         (type exp)))

(defn- get-procedure-type
  [procedure arguments]
  (first procedure))


(defmulti analyse get-exp-type)
(defmulti execute-application get-procedure-type)

(defn analyse-self-evaluating
  [value]
  (fn [env succeed fail]
    (succeed value fail)))
