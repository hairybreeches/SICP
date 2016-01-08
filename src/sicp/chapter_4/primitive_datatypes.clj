(ns sicp.chapter-4.primitive-datatypes
  (:use sicp.chapter-4.evaluator))

(defmethod my-eval java.lang.Number [exp env] exp)
(defmethod my-eval java.lang.String [exp env] exp)
(defmethod my-eval java.lang.Boolean [exp env] exp)
