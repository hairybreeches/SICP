(ns sicp.chapter-4.logic.evaluation)

(def user-initial-environment)

(defn variable? [exp]
  )

(defn binding-in-frame [exp frame]
  )

(defn binding-value [variable]
  )

(defn make-new-variable
  [variable id]  )

(defn exten [variable datum frame])

(defn fetch-assertions [pattern frame]
  )

(defn negated-query [operands]
  )

(defn add-rule-or-assertion [assertion]
  )

(defn add-assertion-body [query]
  )

(defn assertion-to-be-added? [query]
  )

(defn query-syntax-process [input]
  )

(defn contract-question-mark [variable frame]
  )

(defn conclusion
  [rule])

(defn rule-body
  [rule])

(defn fetch-rules
  [pattern frame])


(defmulti qeval (fn [query frames] (type query)))

(defn instantiate [exp frame unbound-var-handler]
  (cond (variable? exp)
        (let [result (binding-in-frame exp frame)]
          (if result
            (instantiate (binding-value result) frame unbound-var-handler)
            (unbound-var-handler exp frame)))

          (seq? exp)
          (map #(instantiate % frame unbound-var-handler) exp)

          :else exp))

