(ns sicp.chapter-4.interpreter.boolean-operators
  (:use sicp.chapter-4.interpreter.if)
  (:use sicp.chapter-4.interpreter.evaluator))

(defn- and->if [tests]
  (if (empty? tests)
      true
      (make-if (first tests)
               (and->if (rest tests))
               false)))

(defn make-not [t]
  (create-expression 'not (list t)))

(defn make-and
  [tests]
  (create-expression 'and tests))

(defmethod analyse 'not [exp]
  (analyse (make-if (first (operands exp)) false true)))

(defmethod analyse 'and [exp]
  (analyse (and->if (operands exp))))

(defmethod analyse 'or [exp]
  (analyse
    (make-not
      (make-and
        (map make-not (operands exp))))))
