(ns sicp.chapter-4.boolean-operators
  (:use sicp.chapter-4.if)
  (:use sicp.chapter-4.evaluator))

(defn- and->if [tests]
  (if (empty? tests)
      true
      (make-if (first tests)
               (and->if (rest tests))
               false)))

(defn- make-not [t]
  (list 'not t))

(defn make-and
  [tests]
  (cons 'and tests))

(defmethod my-eval 'not [exp env]
  (my-eval (make-if (second exp) false true) env))

(defmethod my-eval 'and [exp env]
  (my-eval (and->if (rest exp)) env))

(defmethod my-eval 'or [exp env]
  (my-eval
    (make-not
      (make-and
        (map make-not (rest exp))))
    env))
