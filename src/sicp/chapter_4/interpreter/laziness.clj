(ns sicp.chapter-4.interpreter.laziness
  (:use sicp.chapter-4.interpreter.evaluator))

(def actual-value)

;thunks
(defn- make-thunk
  [exp env]
  ^{:type ::thunk}
  {
    :exp exp
    :env env
    :memo (ref { :evaluated false})
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

;evaluated thunks
(defn- thunk-memo
  [thunk]
  @(:memo thunk))

(defn- thunk-evaluated?
  [thunk]
  (:evaluated (thunk-memo thunk)))

(defn- thunk-value
  [evaluated-thunk]
  (:value (thunk-memo evaluated-thunk)))

(defn- memoise-thunk
  [thunk]
  {:evaluated true
   :value (actual-value (thunk-exp thunk)
                        (thunk-env thunk))})

(defn- process-thunk
  [thunk]
  (dosync
    (ref-set (:memo thunk) (memoise-thunk thunk))))

;laziness
(defmulti force-it type)

(defmethod force-it :default [obj] obj)

(defmethod force-it ::thunk [thunk]
  (if (not (thunk-evaluated? thunk))
      (process-thunk thunk))

  (thunk-value thunk))

(defn delay-it
  [exp env]
  (make-thunk exp env))

(defn actual-value [exp env]
  (force-it (my-eval exp env)))
