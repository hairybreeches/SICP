(ns sicp.chapter-4.interpreter.require
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.primitive-datatypes))

(defn- require-predicate [exp]
  (first (operands exp)))

(defn- analyse-require [exp]
  (let [pred-function (analyse (require-predicate exp))]
    (fn [env succeed fail]
      (pred-function
        env
        (fn [pred-value fail2]
          (if (my-true? pred-value)
            (succeed 'ok fail2)
            (fail)))
        fail))))

(defmethod analyse 'require [exp]
  (analyse-require exp))
