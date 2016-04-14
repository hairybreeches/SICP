(ns sicp.chapter-4.interpreter.if-fail
  (:use sicp.chapter-4.interpreter.evaluator))

(defn- analyse-if-fail [exp]
  (let [initial (analyse (first (operands exp)))
        fallback (analyse (second (operands exp)))]
    (fn [env succeed fail]
      (initial
        env
        succeed
        (fn [] (fallback env succeed fail))))))


(defmethod analyse 'if-fail [exp]
  (analyse-if-fail exp))
