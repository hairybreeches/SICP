(ns sicp.chapter-4.interpreter.quote
  (:use sicp.chapter-4.interpreter.evaluator))

(defn text-of-quotation
  [exp]
  (first (operands exp)))

(defmethod analyse 'quote [exp]
  (let [text (text-of-quotation exp)]
  (analyse-self-evaluating text)))
