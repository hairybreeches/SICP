(ns sicp.chapter-4.interpreter.laziness
  (:use sicp.chapter-4.interpreter.evaluator))


(defn- make-thunk
  [exp env]
  (list 'thunk exp env))

(defn- thunk?
  [exp]
  (and (seq? exp)
       (= (first exp)
          'thunk)))

(defn thunk-exp
  [thunk]
  (second thunk))

(defn thunk-env
  [thunk]
  (nth thunk 2))

(def actual-value)

(defn- force-it
  [obj]
  (if (thunk? obj)
    (actual-value (thunk-exp obj) (thunk-env obj))
    obj))

(defn delay-it
  [exp env]
  (make-thunk exp env))

(defn actual-value [exp env]
  (force-it (my-eval exp env)))
