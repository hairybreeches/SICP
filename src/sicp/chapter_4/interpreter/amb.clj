(ns sicp.chapter-4.interpreter.amb
  (:use sicp.chapter-4.interpreter.evaluator))


(defn- amb-choices [exp]
  (operands exp))

(defn- try-next [choices env succeed fail]
  (if (empty? choices)
      (fail)
      ((first choices)
        env
        succeed
        (fn [] (try-next (rest choices) env succeed fail)))))

(defn- analyse-amb [exp]
  (let [choice-procs (map analyse (amb-choices exp))]
    (fn [env succeed fail]
      (try-next
        choice-procs
        env succeed fail))))

(defn- analyse-ramb [exp]
  (let [choice-procs (map analyse (amb-choices exp))]
    (fn [env succeed fail]
      (try-next
        (shuffle choice-procs)
        env succeed fail))))


(defmethod analyse 'amb [exp]
  (analyse-amb exp))

(defmethod analyse 'ramb [exp]
  (analyse-ramb exp))
