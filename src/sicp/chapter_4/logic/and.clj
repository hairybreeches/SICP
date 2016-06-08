(ns sicp.chapter-4.logic.and
  (:use sicp.chapter-4.logic.rule-stack)
  (:use sicp.chapter-4.logic.frames)
  (:require [schema.core :as s])
  (:use sicp.chapter-4.logic.evaluation))

(defn- analyse-and
  [statements]
  (let [and-functions (map analyse statements)]
    (reduce
      (fn [and-statement-so-far next-statement]

        (s/fn :- Frame-Stream
          [frames :- Frame-Stream
           rule-stack :- Rule-Stack]

          (next-statement
            (and-statement-so-far frames rule-stack)
            rule-stack)))

        and-functions)))


(defmethod analyse-dispatch 'and [_ query-pattern]
  (analyse-and query-pattern))

