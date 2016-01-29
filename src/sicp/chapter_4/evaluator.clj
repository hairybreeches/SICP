(ns sicp.chapter-4.evaluator
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
  [exp env]
  (if (seq? exp)
      (operator exp)
      (type exp)))

(defn- get-procedure-type
  [procedure arguments]
  (first procedure))

;eval/apply
(defmulti my-eval get-exp-type)
(defmulti my-apply get-procedure-type)
