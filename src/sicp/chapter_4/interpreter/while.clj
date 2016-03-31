(ns sicp.chapter-4.interpreter.while
  (:use sicp.chapter-4.interpreter.if)
  (:use sicp.chapter-4.interpreter.begin)
  (:use sicp.chapter-4.interpreter.let)
  (:use sicp.chapter-4.interpreter.evaluator))

(defn- get-body
  [exp]
  (second (operands exp)))

(defn- get-predicate
  [exp]
  (first (operands exp)))

(defn- while->if
  [exp]
  (let [func-name (gensym)]
    (make-named-let
      func-name
      (list)
      (make-if
        (get-predicate exp)
        (sequence->exp
           (list
              (get-body exp)
              (create-expression func-name '())))
        true))))

(defmethod analyse 'while [exp]
  (analyse (while->if exp)))


