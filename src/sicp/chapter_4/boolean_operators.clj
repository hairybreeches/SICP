(ns sicp.chapter-4.boolean-operators
  (:use sicp.chapter-4.primitive-datatypes)
  (:use sicp.chapter-4.evaluator))

(defn- eval-and [tests env]
  (loop [tests tests]
    (cond (empty? tests) true
          (my-false? (my-eval (first tests) env)) false
          :else (recur (rest tests)))))

(defmethod my-eval 'and [exp env]
  (eval-and (rest exp) env))

(defn- eval-or [tests env]
  (loop [tests tests]
    (cond (empty? tests) false
          (my-true? (my-eval (first tests) env)) true
          :else (recur (rest tests)))))

(defmethod my-eval 'or [exp env]
  (eval-or (rest exp) env))
