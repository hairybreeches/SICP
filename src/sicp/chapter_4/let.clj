(ns sicp.chapter-4.let
  (:use sicp.chapter-4.evaluator)
  (:use sicp.chapter-4.lambda))


(defn- get-variable-names
  [exp]
  (map first (second exp)))

(defn- get-variable-values
  [exp]
  (map second (second exp)))

(defn- get-body
  [exp]
  (drop 2 exp))

(defmethod my-eval 'let [exp env]
  (my-eval (cons (make-lambda
                   (get-variable-names exp)
                   (get-body exp))
                 (get-variable-values exp))
           env))
