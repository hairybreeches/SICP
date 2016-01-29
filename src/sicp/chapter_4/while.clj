(ns sicp.chapter-4.while
  (:use sicp.chapter-4.if)
  (:use sicp.chapter-4.begin)
  (:use sicp.chapter-4.evaluator))

(defn- get-body
  [exp]
  (second (operands exp)))

(defn- get-predicate
  [exp]
  (first (operands exp)))

(defn- make-while
  [predicate body]
  (create-expression 'while (list predicate body)))

(defmethod my-eval 'while [exp env]
  (my-eval
    (make-if (get-predicate exp)
             (sequence->exp
               (list
                 (get-body exp)
                 exp))
             true)
    env))


