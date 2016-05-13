(ns sicp.chapter-4.logic.evaluation
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.frames))


(defmulti qeval-dispatch (fn [query-type query frames] query-type))

(defn qeval [query frames]
  (qeval-dispatch
    (query-type query)
    (query-content query)
    frames))

(defn instantiate [exp frame unbound-var-handler]
  (cond (variable? exp)
        (let [result (binding-in-frame exp frame)]
          (if result
            (instantiate (binding-value result) frame unbound-var-handler)
            (unbound-var-handler exp frame)))

          (seq? exp)
          (map #(instantiate % frame unbound-var-handler) exp)

          :else exp))

(defn prettify
  [query frame]
  (remove-dots
    (instantiate query frame (fn [v f] (contract-question-mark v)))))

