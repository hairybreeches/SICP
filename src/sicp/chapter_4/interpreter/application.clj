(ns sicp.chapter-4.interpreter.application
  (:use sicp.chapter-4.interpreter.evaluator))

(defn- no-operands?
  [ops]
  (empty? ops))

(defn- first-operand
  [ops]
  (first ops))

(defn- rest-operands
  [ops]
  (rest ops))

(defn- list-of-values-l-r
  [exps env]
  (if (no-operands? exps)
      '()
      (let [value (my-eval (first-operand exps) env)]
      (cons value
            (list-of-values-l-r (rest-operands exps) env)))))

(defn- list-of-values-r-l
  [exps env]
  (reverse (list-of-values-l-r (reverse exps) env)))

(defn- list-of-values
  [exps env]
  (list-of-values-l-r exps env))

(defmethod my-eval :default [exp env]
  (my-apply
    (my-eval (operator exp) env)
    (list-of-values (operands exp) env)))
