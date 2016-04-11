(ns sicp.chapter-4.interpreter.quote
  (:use sicp.chapter-4.interpreter.lazy-lists)
  (:use sicp.chapter-4.interpreter.evaluator))

(defn text-of-quotation
  [exp]
  (first (operands exp)))

(defmethod my-eval 'quote [exp env]
  (let [result (to-lazy-lists (text-of-quotation exp) env)]
  result))
