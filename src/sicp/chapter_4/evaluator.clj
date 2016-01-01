(ns sicp.chapter-4.evaluator
  (:use sicp.error))

(def my-apply)
(def my-eval)

(def no-operands?)
(defn self-evaluating?
  [exp]
  (cond (number? exp) true
        (string? exp) true
        :else false))

(defn variable?
  [exp]
  (symbol? exp))

(def lookup-variable-value)

(defn tagged-list?
  [exp tag]
  (if (seq? exp)
      (= (first exp) tag)
      false))

(defn quoted?
  [exp]
  (tagged-list? exp 'quote))

(defn text-of-quotation
  [exp]
  (second exp))

(defn lambda?
  [exp]
  (tagged-list? exp 'lambda))

(defn lambda-parameters
  [exp]
  (second exp))

(defn lambda-body
  [exp]
  (drop 2 exp))

(defn make-lambda
  [parameters body]
  (cons 'lambda (cons parameters body)))

(defn assignment?
  [exp]
  (tagged-list? exp 'set!))

(defn assignment-variable
  [exp]
  (second exp))

(defn assignment-value
  [exp]
  (nth exp 2))

(def set-variable-value!)
(defn eval-assignment
  [exp env]
  (set-variable-value!
    (assignment-variable exp)
    (my-eval (assignment-value exp) env)
    env)
  :ok)

(defn definition?
  [exp]
  (tagged-list? exp 'define))

(defn definition-variable
  [exp]
  (if (symbol? (second exp))
      (second exp)
      (first (second exp))))

(defn definition-value
  [exp]
  (if (symbol? (second exp))
      (nth exp 2)
      (make-lambda
        (rest (second exp))
        (drop 2 exp))))

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

(defn list-of-values-l-r
  [exps env]
  (if (no-operands? exps)
      '()
      (let [value (my-eval (first-operand exps) env)]
      (cons value
            (list-of-values-l-r (rest-operands exps) env)))))

(defn list-of-values-r-l
  [exps env]
  (reverse (list-of-values-l-r (reverse exps) env)))

(defn list-of-values
  [exps env]
  (list-of-values-l-r exps env))

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
