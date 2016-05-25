(ns sicp.chapter-4.logic.rules
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.database)
  (:use sicp.chapter-4.logic.unification)
  (:use sicp.sequences))

(defn- rename-variables-in
  ([rule] (rename-variables-in rule (gensym)))
  ([exp id]
   (cond
     (variable? exp) (make-new-variable exp id)
     (non-empty-seq? exp) (cons (rename-variables-in (first exp) id)
                      (rename-variables-in (rest exp) id))
     :else exp)))

(defn- make-stack-layer
  [rule-instance rule-general frame]
  {:rule rule-general
   :rule-values (instantiate (conclusion rule-instance) frame (fn [v f] '?))})

(defn- duplicate-stack-layer? [rule-stack stack-layer]
  (some #{stack-layer} rule-stack))

(defn- apply-a-rule
  [rule pattern frame rule-stack]
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

(defn apply-rules [pattern frame rule-stack]
  (mapcat
    (fn [rule] (apply-a-rule rule pattern frame rule-stack))
    (fetch-rules pattern frame)))
