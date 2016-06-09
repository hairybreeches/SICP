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

(s/defn apply-a-rule
  [rule
   pattern
   frame :- Frame
   rule-stack :- Rule-Stack
   success
   fail]

  (let [clean-rule (rename-variables-in rule)
        unify-result (unify-match pattern
                                  (conclusion clean-rule)
                                  frame)]
    (if (= unify-result 'failed)
      (fail)
      (let [current-stack-layer (make-stack-layer clean-rule rule unify-result)]
        (if (duplicate-stack-layer? rule-stack current-stack-layer)
          (fail)
          ((analyse (rule-body clean-rule))
           unify-result
          (cons current-stack-layer rule-stack)
           success
           fail))))))

(s/defn apply-these-rules
        [rules
         pattern
         frame :- Frame
         rule-stack :- Rule-Stack
         success
         fail]

        (if (empty? rules)
          (fail)
          (apply-a-rule
            (first rules)
            pattern
            frame
            rule-stack
            success
            (fn []
              (apply-these-rules
                (rest rules)
                pattern
                frame
                rule-stack
                success
                fail)))))

(s/defn apply-rules
  [pattern
   frame :- Frame
   rule-stack :- Rule-Stack
   success
   fail]
        (let [rules (fetch-rules pattern frame)]
          (apply-these-rules rules pattern frame rule-stack success fail)))
