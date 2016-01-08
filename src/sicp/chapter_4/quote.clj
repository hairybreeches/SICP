(ns sicp.chapter-4.quote
  (:use sicp.chapter-4.evaluator))

(defn text-of-quotation
  [exp]
  (second exp))

(defmethod my-eval 'quote [exp env]
  (text-of-quotation exp))
