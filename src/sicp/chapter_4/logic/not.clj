(ns sicp.chapter-4.logic.not
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.query-syntax))

(defn- negate [operands frames]
  (mapcat
    (fn [frame]
      (if (empty? (qeval (negated-query operands)
                         (list frame)))
        (list frame)
        '()))
    frames))


(defmethod qeval-dispatch 'not [_ query-pattern frames]
  (negate query-pattern frames))
