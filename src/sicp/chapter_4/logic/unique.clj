(ns sicp.chapter-4.logic.unique
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.error)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.rule-stack)
  (:require [schema.core :as s]))


;I wonder if there's a way to do this without state?
(s/defn analyse-unique [unique-statement]
        (let [unique-function (analyse unique-statement)]
          (s/fn
            [frame :- Frame
             rule-stack :- Rule-Stack
             success
             fail]

            (let [result (ref false)
                  have-received-result (ref false)]

              (unique-function
                frame
                rule-stack
                (fn [this-result
                     fail2]
                  (if @have-received-result
                    (fail) ;there was more than one result
                    (dosync
                      (ref-set result this-result)
                      (ref-set have-received-result true)
                      (fail2) ;see if there are any more results

                      )))

                (fn []
                  (if @have-received-result
                    (success @result fail) ;there was only one result
                    (fail);there were no results
                    )))))))

(defmethod analyse-dispatch 'unique [_ query-pattern]
  (analyse-unique (first query-pattern)))
