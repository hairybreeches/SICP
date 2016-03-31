(ns sicp.chapter-4.interpreter.primitive-datatypes
  (:use sicp.chapter-4.interpreter.evaluator))

(defmethod analyse java.lang.Number [exp] (analyse-self-evaluating exp))
(defmethod analyse java.lang.String [exp] (analyse-self-evaluating exp))

(defn my-false?
  [x]
  (= x false))

(defn my-true?
  [x]
  (not (my-false? x)))

(defmethod analyse java.lang.Boolean [exp] (analyse-self-evaluating exp))
