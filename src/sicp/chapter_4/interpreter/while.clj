(ns sicp.chapter-4.interpreter.while
  (:use sicp.chapter-4.interpreter.if)
  (:use sicp.chapter-4.interpreter.begin)
  (:use sicp.chapter-4.interpreter.evaluator))

(defn- get-body
  [exp]
  (second (operands exp)))

(defn- get-predicate
  [exp]
  (first (operands exp)))

(defn- make-while
  [predicate body]
  (create-expression 'while (list predicate body)))

(defmethod analyse 'while [exp env]
  (my-eval
    (make-if (get-predicate exp)
             (sequence->exp
               (list
                 (get-body exp)
                 exp))
             true)
    env))


