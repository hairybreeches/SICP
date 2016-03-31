(ns sicp.chapter-4.interpreter.amb
  (:use sicp.chapter-4.interpreter.evaluator))


(defn- amb-choices [exp]
  (operands exp))

(defn- analyse-amb [exp]
  (let [choice-procs (map analyse (amb-choices exp))]
    (fn [env succeed fail]
      (let [try-next [choices]
            (if (empty? choices)
                (fail)
                ((first choices)
                 env
                 succeed
                 (fn [] (try-next (rest choices)))))]
        (try-next choice-procs)))))


(defmethod analyse 'amb [exp]
  (analyse-amb exp))
