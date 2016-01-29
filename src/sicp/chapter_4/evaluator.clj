(ns sicp.chapter-4.evaluator
  (:use sicp.chapter-4.procedures)
  (:use sicp.error)
  (:use sicp.chapter-4.environments))

(defn operator
  [exp]
  (first exp))

(defn operands
  [exp]
  (rest exp))

(defn create-expression
  [form operands]
  (cons form operands))

(defn get-exp-type
  [exp env]
  (if (seq? exp)
      (operator exp)
      (type exp)))

(defmulti my-eval get-exp-type)

(defn first-exp
  [exp]
  (first exp))

(defn rest-exps
  [exp]
  (rest exp))

(defn last-exp?
  [exp]
  (empty? (rest-exps exp)))

(defn eval-sequence
  [exps env]
  (loop [exps exps
         env env]
    (if (last-exp? exps)
        (my-eval (first-exp exps) env)
        (do (my-eval (first-exp exps) env)
            (recur (rest-exps exps) env)))))

(defn my-apply
  [procedure arguments]
  (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure) (eval-sequence
                                         (procedure-body procedure)
                                         (extend-environment (procedure-parameters procedure)
                                                             arguments
                                                             (procedure-environment procedure)))
        :else (error "Unknown procedure type: " procedure)))
