(ns sicp.chapter-4.logic.rules
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.database)
  (:use sicp.chapter-4.logic.rule-stack)
  (:use sicp.chapter-4.logic.unification)
  (:use sicp.sequences)
  (:require [schema.core :as s]))

(defn- rename-variables-in
  ([rule] (rename-variables-in rule (gensym)))
  ([exp id]
   (cond
     (variable? exp) (make-new-variable exp id)
     (non-empty-seq? exp) (cons (rename-variables-in (first exp) id)
                      (rename-variables-in (rest exp) id))
     :else exp)))

(s/defn apply-a-rule :- Frame-Stream
  [rule
   pattern
   frame :- Frame
   rule-stack :- Rule-Stack]
  (let [clean-rule (rename-variables-in rule)
        unify-result (unify-match pattern
                                  (conclusion clean-rule)
                                  frame)]
    (if (= unify-result 'failed)
      '()
      (let [current-stack-layer (make-stack-layer clean-rule rule unify-result)]
        (if (duplicate-stack-layer? rule-stack current-stack-layer)
          '()
          (qeval (rule-body clean-rule)
                 (list unify-result)
                 (cons current-stack-layer rule-stack)))))))

(s/defn apply-rules :- Frame-Stream
  [pattern
   frame :- Frame
   rule-stack :- Rule-Stack]
  (mapcat
    (fn [rule] (apply-a-rule rule pattern frame rule-stack))
    (fetch-rules pattern frame)))
