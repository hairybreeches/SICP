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

(defmethod analyse 'while [exp]
  (let [action (analyse (get-body exp))
        predicate (analyse (get-predicate exp))]
      (fn [env]
        (loop []
          (if (predicate env)
              (do (action env)
                  (recur))
              true)))))


