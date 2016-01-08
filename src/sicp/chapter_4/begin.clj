(ns sicp.chapter-4.begin
  (:use sicp.chapter-4.evaluator))

(defn- begin-actions
  [exp]
  (rest exp))

(defn- make-begin
  [actions]
  (cons 'begin actions))

(defn sequence->exp
  [actions]
  (cond (empty? actions) actions
        (last-exp? actions) (first-exp actions)
        :else (make-begin actions)))

(defmethod my-eval 'begin [exp env]
  (eval-sequence
    (begin-actions exp)
    env))
