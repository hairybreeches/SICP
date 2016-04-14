(ns sicp.chapter-4.logic.querier
  (:use sicp.chapter-4.logic.simple-queries)
  (:use sicp.chapter-4.logic.clojure-value)
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

(defn- add-rule-or-assertion [assertion]
  )

(defn- add-assertion-body [query]
  )

(defn- assertion-to-be-added? [query]
  )

(defn- query-syntax-process [input]
  )

(defn- contract-question-mark [variable frame]
  )

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

