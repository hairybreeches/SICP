(ns sicp.chapter-4.evaluator
  (:use sicp.error))

(def my-apply)
(def my-eval)

(def no-operands?)
(def self-evaluating?)

(def variable?)
(def lookup-variable-value)

(def quoted?)
(def text-of-quotation)

(def assignment?)
(def assignment-value)
(def assignment-variable)
(def set-variable-value!)
(defn eval-assignment
  [exp env]
  (set-variable-value!
    (assignment-variable exp)
    (my-eval (assignment-value exp) env)
    env)
  :ok)

(def definition?)
(def definition-variable)
(def definition-value)
(def define-variable!)
(defn eval-definition
  [exp env]
  (define-variable!
    (definition-variable exp)
    (my-eval (definition-value exp) env)
    env)
  :ok)



(def begin?)
(def begin-actions)

(def application?)
(def operator)
(def operands)
(def first-operand)
(def rest-operands)

(def primitive-procedure?)
(def apply-primitive-procedure)
(def compound-procedure?)

(def make-procedure)
(def procedure-body)
(def procedure-parameters)
(def procedure-environment)

(def lambda?)
(def lambda-parameters)
(def lambda-body)

(def cond?)
(def cond->if)

(def extend-environment)

(def if?)
(def if-predicate)
(def if-consequent)
(def if-alternative)

(defn eval-if
  [exp env]
  (if (true? (my-eval (if-predicate exp) env))
    (my-eval (if-consequent exp) env)
    (my-eval (if-alternative exp) env)))

(def first-exp)
(def rest-exps)
(def last-exp?)

(defn eval-sequence
  [exps env]
  (loop [exps exps
         env env]
    (if (last-exp? exps)
        (eval (first-exp exps) env)
        (do (eval (first-exp exps) env)
            (recur (rest-exps exps) env)))))

(defn list-of-values
  [exps env]
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


(defn my-eval
  [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp) (lookup-variable-value exp env)
        (quoted? exp) (text-of-quotation exp)
        (assignment? exp) (eval-assignment exp env)
        (definition? exp) (eval-definition exp env)
        (if? exp) (eval-if exp env)
        (lambda? exp) (make-procedure (lambda-parameters exp)
                                      (lambda-body exp)
                                      env)
        (begin? exp) (eval-sequence (begin-actions exp)
                                    env)
        (cond? exp) (my-eval (cond->if exp) env)
        (application? exp) (my-apply (my-eval (operator exp) env)
                                     (list-of-values (operands exp) env))
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
