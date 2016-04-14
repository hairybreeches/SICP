(ns sicp.chapter-4.logic.and
  (:use sicp.chapter-4.logic.evaluation))

(defn- conjoin [conjuncts frames]
  (loop [conjuncts conjuncts
         frames frames]
    (if (empty? conjuncts)
        frames
      (recur (rest conjuncts)
             (qeval (first conjuncts)
                    frames)))))

(defmethod qeval :and [query-pattern frames]
  (conjoin query-pattern frames))

