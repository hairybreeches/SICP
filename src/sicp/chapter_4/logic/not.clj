(ns sicp.chapter-4.logic.not
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.frames)
  (:require [schema.core :as s]))

(s/defn negate :- Frame-Stream
  [operands frames :- Frame-Stream rule-stack]
  (mapcat
    (fn [frame]
      (add-filter
        frame
        (negated-query operands)
        #(empty? (qeval % (create-empty-frame) rule-stack))))

    frames))


(defmethod qeval-dispatch 'not [_ query-pattern frames rule-stack]
  (negate query-pattern frames rule-stack))
