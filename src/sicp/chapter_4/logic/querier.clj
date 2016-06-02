(ns sicp.chapter-4.logic.querier
  (:use sicp.chapter-4.logic.simple-queries)
  (:use sicp.chapter-4.logic.clojure-value)
  (:use sicp.chapter-4.logic.and)
  (:use sicp.chapter-4.logic.or)
  (:use sicp.chapter-4.logic.always-true)
  (:use sicp.chapter-4.logic.not)
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.database)
  (:use sicp.chapter-4.logic.unique)
  (:use sicp.chapter-4.logic.frames)
  (:require [schema.core :as s]))

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

(defn- execute-expression
  [exp]
  (let [query (query-syntax-process exp)]
      (cond (assertion-to-be-added? query)
            (do
              (add-rule-or-assertion (add-assertion-body query))
              "Assertion added to data base")

            :else
             (map
                  #(prettify query %)
                  (qeval query (list (create-empty-frame)))))))

(defn query-driver-loop []
  (s/with-fn-validation
    (loop []
      (prompt-for-input)
      (let [input (read)]
        (announce-output (execute-expression input))
        (recur)))))

(defn execute-query [data query]
  (s/with-fn-validation
    (clear-database)
    (load-database data)
    (execute-expression query)))

