(ns sicp.chapter-4.interpreter.laziness
  (:use sicp.chapter-4.interpreter.evaluator))

(def actual-value)

;thunks
(defn- make-memoising-thunk
  [exp env]
  ^{:type ::memoising-thunk}
  {
    :exp exp
    :env env
    :memo (ref { :evaluated false})
    })

(defn- make-recalculating-thunk
  [exp env]
  ^{:type ::recalculating-thunk}
  {
    :exp exp
    :env env
    })

(defn- thunk?
  [exp]
  (= (type exp) ::thunk))

(defn thunk-exp
  [thunk]
  (:exp thunk))

(defn thunk-env
  [thunk]
  (:env thunk))

(defn- thunk-memo
  [thunk]
  @(:memo thunk))

(defn- thunk-evaluated?
  [thunk]
  (:evaluated (thunk-memo thunk)))

(defn- thunk-memoised-value
  [evaluated-thunk]
  (:value (thunk-memo evaluated-thunk)))

(defn- calculate-thunk-value
  [thunk]
  (actual-value (thunk-exp thunk)
                (thunk-env thunk)))

(defn- memoise-thunk
  [thunk]
  {:evaluated true
   :value (calculate-thunk-value thunk)})

(defn- process-thunk
  [thunk]
  (dosync
    (ref-set (:memo thunk) (memoise-thunk thunk))))

;laziness
(defmulti force-it type)

(defmethod force-it :default [obj] obj)

(defmethod force-it ::memoising-thunk [thunk]
  (if (not (thunk-evaluated? thunk))
      (process-thunk thunk))

  (thunk-memoised-value thunk))

(defmethod force-it ::recalculating-thunk [thunk]
  (calculate-thunk-value thunk))

(defn delay-it
  [exp env]
  (make-memoising-thunk exp env))

(defn delay-it-no-memo
  [exp env]
  (make-recalculating-thunk exp env))

(defn actual-value [exp env]
  (force-it (my-eval exp env)))
