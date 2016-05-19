(ns sicp.chapter-4.logic.unique
  (:use sicp.chapter-4.logic.evaluation))


(defn- find-unique [pattern frame rule-stack]
  (let [result (qeval pattern (list frame) rule-stack)]
    (if (= (count result) 1)
      result
      '())))

(defmethod qeval-dispatch 'unique [_ query-pattern frames rule-stack]
  (mapcat #(find-unique (first query-pattern) % rule-stack) frames))
