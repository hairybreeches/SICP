(ns sicp.chapter-4.interpreter.if
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.laziness)
  (:use sicp.chapter-4.interpreter.primitive-datatypes))

(defn- if-predicate
  [exp]
  (first (operands exp)))

(defn- if-consequent
  [exp]
  (second (operands exp)))

(defn- if-alternative
  [exp]
  (if (empty? (drop 2 (operands exp)))
      false
      (nth (operands exp) 2)))

(defn- eval-if
  [exp env]
  (if (my-true? (actual-value (if-predicate exp) env))
    (my-eval (if-consequent exp) env)
    (my-eval (if-alternative exp) env)))

(defn make-if
  ([predicate consequent alternative]
    (create-expression 'if (list predicate consequent alternative)))
  ([predicate consequent]
    (create-expression 'if (list predicate consequent))))

(defmethod my-eval 'if [exp env]
  (eval-if exp env))
