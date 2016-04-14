(ns sicp.chapter-4.logic.querier
  (:use sicp.chapter-4.logic.simple-queries)
  (:use sicp.chapter-4.logic.and)
  (:use sicp.chapter-4.logic.or)
  (:use sicp.chapter-4.logic.not)
  (:use sicp.chapter-4.logic.evaluation))

(def input-prompt ";;; Query input:")
(def output-prompt ";;; Query results:")

(defn- prompt-for-input
  []
  (prn)
  (prn)
  (prn input-prompt))

(defn- announce-output
  []
  (prn)
  (prn output-prompt))

(defn- acknowledge-rule
  []
  (prn)
  (prn "Assertion added to data base"))

(defn binding-in-frame [exp frame]
  )

(defn variable? [exp]
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


(defn variable-value [variable]
  )

(defn instantiate [exp frame unbound-var-handler]
  (cond (variable? exp)
        (let [result (binding-in-frame exp frame)]
          (if result
            (instantiate (variable-value result) frame unbound-var-handler)
            (unbound-var-handler exp frame)))

          (seq? exp)
          (map #(instantiate % frame unbound-var-handler) exp)

          :else exp))


(defn query-driver-loop []
  (loop []
    (prompt-for-input)
    (let [query (query-syntax-process (read))]
      (cond (assertion-to-be-added? query)
            (do
              (add-rule-or-assertion (add-assertion-body query))
              (acknowledge-rule)
              (recur))

            :else
            (do
              (announce-output)
              (prn
                (map
                  #(instantiate query % (fn [v f] (contract-question-mark v)))
                  (qeval query '(()))))
              (recur))))))

(defn execute-query [data query]
  "")

