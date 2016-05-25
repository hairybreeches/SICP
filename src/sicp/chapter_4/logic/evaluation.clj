(ns sicp.chapter-4.logic.evaluation
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.frames))


(defmulti qeval-dispatch (fn [query-type query frames rule-stack] query-type))

(defn qeval
  ([query frames]
   (qeval query frames '()))

  ([query frames rule-stack]

   (qeval-dispatch
    (query-type query)
    (query-content query)
    frames
     rule-stack)))

(defn prettify
  [query frame]
  (remove-dots
    (instantiate query frame (fn [v f] (contract-question-mark v)))))

