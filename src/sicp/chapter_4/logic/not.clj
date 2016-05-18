(ns sicp.chapter-4.logic.not
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.query-syntax))

(defn- negate [operands frames rule-stack]
  (mapcat
    (fn [frame]
      (if (empty? (qeval (negated-query operands)
                         (list frame)
                         rule-stack))
        (list frame)
        '()))
    frames))


(defmethod qeval-dispatch 'not [_ query-pattern frames rule-stack]
  (negate query-pattern frames rule-stack))
