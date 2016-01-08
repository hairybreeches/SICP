(ns sicp.chapter-4.if
  (:use sicp.chapter-4.evaluator)
  (:use sicp.chapter-4.primitive-datatypes))

(defn- if-predicate
  [exp]
  (second exp))

(defn- if-consequent
  [exp]
  (nth exp 2))

(defn- if-alternative
  [exp]
  (if (empty? (drop 3 exp))
      false
      (nth exp 3)))

(defn- eval-if
  [exp env]
  (if (my-true? (my-eval (if-predicate exp) env))
    (my-eval (if-consequent exp) env)
    (my-eval (if-alternative exp) env)))

(defn make-if
  [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defmethod my-eval 'if [exp env]
  (eval-if exp env))
