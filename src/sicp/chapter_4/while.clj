(ns sicp.chapter-4.while
  (:use sicp.chapter-4.if)
  (:use sicp.chapter-4.begin)
  (:use sicp.chapter-4.evaluator))

(defn- get-body
  [exp]
  (nth exp 2))

(defn- get-predicate
  [exp]
  (second exp))

(defn- make-while
  [predicate body]
  (list 'while predicate body))

(defmethod my-eval 'while [exp env]
  (my-eval
    (make-if (get-predicate exp)
             (sequence->exp
               (list
                 (get-body exp)
                 exp))
             true)
    env))


