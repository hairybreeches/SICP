(ns sicp.chapter-4.logic.frames
  (:use sicp.chapter-4.logic.query-syntax)
  (:require [schema.core :as s]))


(def Binding
  "schema for a variable binding"
  {:value s/Any :variable s/Any})

(def Filter
  "schema for a filter"
  {:pattern s/Any :predicate s/Any})

(def Frame
  "schema for a frame"
  {
    :filters [Filter]
    s/Any Binding
    })

(def Frame-Stream
  "schema for a sequnce of frames"
  [Frame])

(s/defn binding-value
        [bind :- Binding]
  (:value bind))

(s/defn binding-variable
  [bind :- Binding]
  (:variable bind))

(s/defn make-binding :- Binding
  [variable value]
  {:variable variable :value value})

(s/defn binding-in-frame
  [k frame :- Frame]
  (frame k))

(s/defn try-instantiate
        [exp frame :- Frame unbound-var-handler]
  (cond (variable? exp)
        (let [result (binding-in-frame exp frame)]
          (if result
            (try-instantiate (binding-value result) frame unbound-var-handler)
            {:success false :result (unbound-var-handler exp frame)}))

        (seq? exp)
        (let [results (map #(try-instantiate % frame unbound-var-handler) exp)]
          {:result (map :result results)
           :success (every? :success results)})

        :else {:result exp :success true}))

(defn instantiate [exp frame unbound-var-handler]
  (:result (try-instantiate exp frame unbound-var-handler)))

(s/defn make-filter :- Filter
  [pattern predicate]
  {:pattern pattern :predicate predicate})

(defn- try-evaluate-filter
  [filt frame]
  (let [pattern-result (try-instantiate (:pattern filt) frame (fn [_ __] '?))]
    (if (:success pattern-result)
      (if ((:predicate filt) (:result pattern-result))
        :success
        :failure)
      :unknown)))

(defn- force-evaluate-filter
  [filt frame]
  ((:predicate filt) (instantiate (:pattern filt) frame (fn [var _] var))))

(s/defn filters-all-valid :- s/Bool
        [frame :- Frame]
        (every? #(force-evaluate-filter % frame) (:filters frame)))



(defn- evaluate-filters [frame]
  (let [results (group-by #(try-evaluate-filter % frame) (:filters frame))]
    (if (contains? results :failure)
      'failed
      (assoc frame :filters (:unknown results)))))

(s/defn extend-frame
  [variable datum frame :- Frame]
  (evaluate-filters (assoc frame variable (make-binding variable datum))))

(s/defn create-empty-frame :- Frame
        []
  {:filters '()})

(defn get-all-bindings [frame]
  (map
    second
    (filter #(not (= :filters (first %))) frame)))

(defn add-filter [frame pattern predicate]
  (let [filt (make-filter pattern predicate)
        result (try-evaluate-filter filt frame)]
    (cond (= result :success) (list frame)
          (= result :failure) '()
          (= result :unknown) (list (update-in frame [:filters] #(cons filt %))))))


