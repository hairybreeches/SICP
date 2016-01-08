(ns sicp.chapter-4.evaluator
  (:use sicp.chapter-4.compound-procedures)
  (:use sicp.error)
  (:use sicp.chapter-4.environments))

(def my-apply)
(def my-eval)

(defn get-form [exp env]
  (first exp))

(defmulti eval-list-expression get-form)

(defn get-exp-type
  [exp env]
  (type exp))

(defmulti my-eval get-exp-type)

(defmethod my-eval :default [exp env]
  (if
    (seq? exp)
    (eval-list-expression exp env)
    (error "Unrecognised expression type: " exp)))

(defn primitive-procedure?
  [procedure]
  false)

(def apply-primitive-procedure)

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

(defn execute
  [exp] (my-eval exp (extend-environment '() '() the-empty-environment)))


(defn my-apply
  [procedure arguments]
  (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure) (eval-sequence
                                         (procedure-body procedure)
                                         (extend-environment (procedure-parameters procedure)
                                                             arguments
                                                             (procedure-environment procedure)))
        :else (error "Unknown procedure type: " procedure)))
