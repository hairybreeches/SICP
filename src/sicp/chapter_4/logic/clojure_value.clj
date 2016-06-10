(ns sicp.chapter-4.logic.clojure-value
  (:use sicp.error)
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.rule-stack)
  (:require [schema.core :as s]))

(defn name-before
  [n1 n2]
  (> (compare (name (first n1)) (name (first n2))) 0))

(defn- execute [exp]
  (apply (eval (predicate exp))
         (args exp)))

(s/defn
  evaluate
  [call
   frame :- Frame]
  (execute
    (instantiate
      call
      frame
      (fn [v f]
        (error "unknown pat var: " v)))))

(s/defn
  analyse-clojure-value
  [exp]
  (s/fn
    [frame
     rule-stack :- Rule-Stack
     succeed
     fail]
    (if (evaluate exp frame)
      (succeed frame fail)
      (fail))))

(defmethod analyse-dispatch 'clojure-value [_ query-pattern]
  (analyse-clojure-value query-pattern))
