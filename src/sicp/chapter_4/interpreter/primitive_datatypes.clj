(ns sicp.chapter-4.interpreter.primitive-datatypes
  (:use sicp.chapter-4.interpreter.evaluator))

(defmethod my-eval java.lang.Number [exp env] exp)
(defmethod my-eval java.lang.String [exp env] exp)

(defn my-false?
  [x]
  (= x false))

(defn my-true?
  [x]
  (not (my-false? x)))

(defmethod my-eval java.lang.Boolean [exp env] exp)
