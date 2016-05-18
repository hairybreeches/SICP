(ns sicp.chapter-4.logic.rules
  (:use sicp.sequences)
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.database)
  (:use sicp.sequences))

(def unify-match)

(defn- depends-on?
  [exp variable frame]
  (cond (variable? exp) (if (= variable exp)
                          true
                          (let [b (binding-in-frame exp frame)]
                            (if b
                              (depends-on? (binding-value b) variable frame)
                              false)))
        (non-empty-seq? exp) (or (depends-on? (first exp) variable frame)
                       (depends-on? (rest exp) variable frame))
        :else false))

(defn- extend-if-possible
  [variable value frame]
  (let [bind (binding-in-frame variable frame)]
    (cond bind (unify-match (binding-value bind) value frame)
          (variable? value) (let [bind2 (binding-in-frame value frame)]
                              (if bind2
                                (unify-match variable (binding-value bind2) frame)
                                (exten variable value frame)))
          (depends-on? value variable frame) 'failed
          :else (exten variable value frame))))

(defn- unify-match
  [pattern1 pattern2 frame]
  (cond
    (= frame 'failed) 'failed
    (= pattern1 pattern2) frame
    (variable? pattern1) (extend-if-possible pattern1 pattern2 frame)
    (variable? pattern2) (extend-if-possible pattern2 pattern1 frame)
    (and (non-empty-seq? pattern1)
         (non-empty-seq? pattern2)) (unify-match
                            (scheme-rest pattern1)
                            (scheme-rest pattern2)
                            (unify-match
                              (first pattern1)
                              (first pattern2)
                              frame))
    :else 'failed))



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
