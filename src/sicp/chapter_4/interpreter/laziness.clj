(ns sicp.chapter-4.interpreter.laziness
  (:use sicp.chapter-4.interpreter.evaluator))

(def actual-value)

(defn- referenced-list?
  [exp tag]
  (and (= (type exp) clojure.lang.Ref)
       (seq? @exp)
       (= (first @exp) tag)))

;thunks
(defn- make-thunk
  [exp env]
  (ref
    (list 'thunk exp env)))

(defn- thunk?
  [exp]
  (referenced-list? exp 'thunk))

(defn thunk-exp-from-value
  [thunk]
  (second thunk))

(defn thunk-env-from-value
  [thunk]
  (nth thunk 2))

;evaluated thunks
(defn- evaluated-thunk?
  [exp]
  (referenced-list? exp 'evaluated-thunk))

(defn- thunk-value
  [evaluated-thunk]
  (second @evaluated-thunk))

(defn- evaluate-thunk
  [thunk]
  (list 'evaluated-thunk (actual-value (thunk-exp-from-value thunk)
                                       (thunk-env-from-value thunk))))

(defn- process-thunk
  [thunk]
  (dosync
    (alter thunk evaluate-thunk)))

;laziness
(defn- force-it
  [obj]
  (cond (thunk? obj) (do (process-thunk obj) (thunk-value obj))
        (evaluated-thunk? obj) (thunk-value obj)
        :else obj))

(defn delay-it
  [exp env]
  (make-thunk exp env))

(defn actual-value [exp env]
  (force-it (my-eval exp env)))
