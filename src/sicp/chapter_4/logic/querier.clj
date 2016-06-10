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
(def new-problem ";;; Starting a new problem")
(def end-problem ";;; There are no more values of:")
(def no-problem ";;; There is no current problem")

(defn- prompt-for-input
  []
  (prn)
  (prn)
  (prn input-prompt))

(defn- announce-output
  []
  (prn)
  (prn output-prompt))

(defn- announce-new-problem
  []
  (prn)
  (prn new-problem))

(defn- announce-end
  [input]
  (prn)
  (prn end-problem)
  (prn input))

(defn- announce-no-problem
  []
  (prn)
  (prn no-problem))



(defn- execute-expression
  ([data query success fail]
   (s/with-fn-validation
     (clear-database)
     (load-database data)
     (execute-expression query success fail)))

  ([exp success fail]
   (let [query (query-syntax-process exp)]
     (cond (assertion-to-be-added? query)
           (do
             (add-rule-or-assertion (add-assertion-body query))
             (success "Assertion added to data base" fail))

           :else
             ((analyse query)
              (create-empty-frame)
              '()
              (fn [frame fail2]
                (success (prettify query frame) fail2))
              fail)))))

(defn- iterate-over-results
  [state]
  ((:try-again @state))
  (if
    (:success @state)
    (cons (:current-result @state) (lazy-seq (iterate-over-results state)))
    '()))

;surely there's a better way?
(defn execute-query
  [data query]
  (let [state (ref false)]
    (dosync
      (ref-set
        state
        {
          :try-again
          (fn [] (execute-expression
                   data
                   query
                   (fn [result do-next]
                     (dosync
                       (ref-set state
                                {
                                  :current-result result
                                  :try-again do-next
                                  :success true
                                  })))
                   (fn []
                     (dosync
                       (ref-set state { :success false })))))}))

    (iterate-over-results state)))

(defn driver-loop

  ([]
   (driver-loop
     (fn [] (announce-no-problem) (driver-loop))))

  ([try-again]
      (prompt-for-input)
      (let [input (read)]
        (if
          (= input 'try-again)
          (try-again)
          (do
            (announce-new-problem)
            (execute-expression
              input
              (fn [value next-alternative]
                (announce-output)
                (prn value)
                (driver-loop next-alternative))
              (fn []
                (announce-end input)
                (driver-loop))))))))
