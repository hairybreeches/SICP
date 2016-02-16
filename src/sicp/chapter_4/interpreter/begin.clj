(ns sicp.chapter-4.interpreter.begin
  (:use sicp.chapter-4.interpreter.evaluator))

(defn- begin-actions
  [exp]
  (operands exp))

(defn- make-begin
  [actions]
  (create-expression 'begin actions))

(defn- first-exp
  [exp]
  (first exp))

(defn- rest-exps
  [exp]
  (rest exp))

(defn- last-exp?
  [exp]
  (empty? (rest-exps exp)))

(defn eval-sequence
  [exps env]
  (loop [exps exps
         env env]
    (if (last-exp? exps)
        (my-eval (first-exp exps) env)
        (do (my-eval (first-exp exps) env)
            (recur (rest-exps exps) env)))))

(defn sequence->exp
  [actions]
  (cond (empty? actions) actions
        (last-exp? actions) (first-exp actions)
        :else (make-begin actions)))

(defmethod my-eval 'begin [exp env]
  (eval-sequence
    (begin-actions exp)
    env))
