(ns sicp.chapter-4.logic.and
  (:use sicp.chapter-4.logic.rule-stack)
  (:use sicp.chapter-4.logic.frames)
  (:require [schema.core :as s])
  (:use sicp.chapter-4.logic.evaluation))


(defn- combine-and-functions
  [and-functions]
  (if (empty? and-functions)
    (s/fn
      [frame :- Frame
       rule-stack :- Rule-Stack
       succeed
       fail]
      (succeed frame fail))

    (let [rest-function (combine-and-functions (rest and-functions))]
      (s/fn
        [frame :- Frame
         rule-stack :- Rule-Stack
         succeed
         fail]

        ((first and-functions)
         frame
         rule-stack
         (s/fn [frame :- Frame
                fail2]

           (rest-function frame rule-stack succeed fail2))
           fail)))))


(defn- analyse-and
  [statements]
  (combine-and-functions (map analyse statements)))

(defmethod analyse-dispatch 'and [_ query-pattern]
  (analyse-and query-pattern))

