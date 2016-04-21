(ns sicp.chapter-4.logic.evaluation)

(def user-initial-environment)

(defmulti qeval (fn [query frames] (type query)))

(defn variable? [exp]
  )

(defn binding-in-frame [exp frame]
  )

(defn binding-value [variable]
  )

(defn instantiate [exp frame unbound-var-handler]
  (cond (variable? exp)
        (let [result (binding-in-frame exp frame)]
          (if result
            (instantiate (binding-value result) frame unbound-var-handler)
            (unbound-var-handler exp frame)))

          (seq? exp)
          (map #(instantiate % frame unbound-var-handler) exp)

          :else exp))

