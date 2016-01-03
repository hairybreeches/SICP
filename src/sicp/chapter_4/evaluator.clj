(ns sicp.chapter-4.evaluator
  (:use sicp.error))

(def my-apply)
(def my-eval)

(defn get-form [exp env]
  (first exp))

(defmulti eval-list-expression get-form)

(defn self-evaluating?
  [exp]
  (cond (number? exp) true
        (string? exp) true
        (= true exp) true
        (= false exp) true
        :else false))

(defn variable?
  [exp]
  (symbol? exp))

(def lookup-variable-value)

(defn first-exp
  [exp]
  (first exp))

(defn rest-exps
  [exp]
  (rest exp))

(defn last-exp?
  [exp]
  (empty? (rest-exps exp)))

(def primitive-procedure?)
(def apply-primitive-procedure)
(def compound-procedure?)

(def make-procedure)
(def procedure-body)
(def procedure-parameters)
(def procedure-environment)

(def extend-environment)

(defn eval-sequence
  [exps env]
  (loop [exps exps
         env env]
    (if (last-exp? exps)
        (my-eval (first-exp exps) env)
        (do (my-eval (first-exp exps) env)
            (recur (rest-exps exps) env)))))

(defn my-eval
  [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp) (lookup-variable-value exp env)
        (seq? exp) (eval-list-expression exp env)
        :else (error "Unrecognised expression type: " exp)))

(defn my-apply
  [procedure arguments]
  (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure) (eval-sequence
                                         (procedure-body procedure)
                                         (extend-environment (procedure-parameters procedure)
                                                             arguments
                                                             (procedure-environment procedure)))
        :else (error "Unknown procedure type: " procedure)))
