(ns sicp.chapter-4.evaluator
  (:use sicp.error))

(def my-apply)
(def my-eval)

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



(defn begin?
  [exp]
  (tagged-list? exp 'begin))

(defn begin-actions
  [exp]
  (rest exp))


(defn first-exp
  [exp]
  (first exp))

(defn rest-exps
  [exp]
  (rest exp))

(defn last-exp?
  [exp]
  (empty? (rest-exps exp)))

(defn make-begin
  [actions]
  (cons 'begin actions))

(defn sequence->exp
  [actions]
  (cond (empty? actions) actions
        (last-exp? actions) (first-exp actions)
        :else (make-begin actions)))

(defn application?
  [exp]
  (tagged-list? exp 'call))

(defn operator
  [exp]
  (second exp))

(defn operands
  [exp]
  (drop 2 exp))

(defn no-operands?
  [ops]
  (empty? ops))

(defn first-operand
  [ops]
  (first ops))

(defn rest-operands
  [ops]
  (rest ops))

(def primitive-procedure?)
(def apply-primitive-procedure)
(def compound-procedure?)

(def make-procedure)
(def procedure-body)
(def procedure-parameters)
(def procedure-environment)

(defn if?
  [exp]
  (tagged-list? exp 'if))

(defn if-predicate
  [exp]
  (second exp))

(defn if-consequent
  [exp]
  (nth exp 2))

(defn if-alternative
  [exp]
  (if (empty? (drop 3 exp))
      false
      (nth exp 3)))

(defn make-if
  [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defn my-false?
  [x]
  (= x false))

(defn my-true?
  [x]
  (not (my-false? x)))

(defn eval-if
  [exp env]
  (if (my-true? (my-eval (if-predicate exp) env))
    (my-eval (if-consequent exp) env)
    (my-eval (if-alternative exp) env)))

(defn cond?
  [exp]
  (tagged-list? exp 'cond))

(defn cond-clauses
  [exp]
  (rest exp))

(defn cond-predicate
  [clause]
  (first clause))

(defn cond-actions
  [clause]
  (rest clause))

(defn cond-else-clause?
  [clause]
  (= (cond-predicate clause) 'else))

(defn expand-clauses
  [clauses]
  (if
    (empty? clauses)
    false
    (let [first-clause (first clauses)
          rest-clauses (rest clauses)]
      (if (cond-else-clause? first-clause)
          (if (empty? rest-clauses)
              (sequence->exp (cond-actions first-clause))
              (error "else clause not last: " clauses))
          (make-if (cond-predicate first-clause)
                   (sequence->exp (cond-actions first-clause))
                   (expand-clauses rest-clauses))))))

(defn cond->if
  [exp]
  (expand-clauses (cond-clauses exp)))

(def extend-environment)

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
        (application? exp) (my-apply (my-eval (operator exp) env)
                                     (list-of-values (operands exp) env))
        (assignment? exp) (eval-assignment exp env)
        (definition? exp) (eval-definition exp env)
        (if? exp) (eval-if exp env)
        (lambda? exp) (make-procedure (lambda-parameters exp)
                                      (lambda-body exp)
                                      env)
        (begin? exp) (eval-sequence (begin-actions exp)
                                    env)
        (cond? exp) (my-eval (cond->if exp) env)
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
