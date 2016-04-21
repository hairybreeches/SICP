(ns sicp.chapter-4.logic.not
  (:use sicp.chapter-4.logic.evaluation))

(defn- negate [operands frames]
  (mapcat
    (fn [frame]
      (if (empty? (qeval (negated-query operands)
                         (list frame)))
        (list frame)
        '()))
    frames))


(defmethod qeval :not [query-pattern frames]
  (negate query-pattern frames))
