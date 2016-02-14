(ns sicp.chapter-4.interpreter.boolean-operators
  (:use sicp.chapter-4.interpreter.if)
  (:use sicp.chapter-4.interpreter.evaluator))

(defn- and->if [tests]
  (if (empty? tests)
      true
      (make-if (first tests)
               (and->if (rest tests))
               false)))

(defn- make-not [t]
  (create-expression 'not (list t)))

(defn make-and
  [tests]
  (create-expression 'and tests))

(defmethod my-eval 'not [exp env]
  (my-eval (make-if (first (operands exp)) false true) env))

(defmethod my-eval 'and [exp env]
  (my-eval (and->if (operands exp)) env))

(defmethod my-eval 'or [exp env]
  (my-eval
    (make-not
      (make-and
        (map make-not (operands exp))))
    env))
