(ns sicp.chapter-4.application
  (:use sicp.chapter-4.evaluator))

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

(defmethod eval-list-expression :default [exp env]
  (my-apply
    (my-eval (operator exp) env)
    (list-of-values (operands exp) env)))
