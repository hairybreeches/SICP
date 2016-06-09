(ns sicp.chapter-4.logic.or
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.sequences)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.rule-stack)
  (:require [schema.core :as s]))

(defn- combine-or-functions
  [or-functions]
  (if (empty? or-functions)
    (s/fn
      [frame :- Frame
       rule-stack :- Rule-Stack
       success
       fail]
      (fail))

    (let [rest-function (combine-or-functions (rest or-functions))]
          (s/fn
      [frame :- Frame
       rule-stack :- Rule-Stack
       success
       fail]

      ((first or-functions)
       frame
       rule-stack
       success
       (fn [] (rest-function frame rule-stack success fail)))))))


(s/defn analyse-or
        [or-statements]
        (combine-or-functions (map analyse or-statements)))

(defmethod analyse-dispatch 'or [_ query-pattern]
  (analyse-or query-pattern))
