(ns sicp.chapter-4.logic.frames
  (:use sicp.chapter-4.logic.query-syntax))

(defn binding-value [bind]
  (:value bind))

(defn binding-variable
  [bind]
  (:variable bind))

(defn make-binding
  [variable value]
  {:variable variable :value value})

(defn binding-in-frame [k frame]
  (frame k))

(defn- try-instantiate [exp frame unbound-var-handler]
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

(defn- make-filter
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

(defn- evaluate-filters [frame]
  (let [results (group-by #(try-evaluate-filter % frame) (:filters frame))]
    (if (contains? results :failure)
      'failed
      (assoc frame :filters (:unknown results)))))

(defn extend-frame [variable datum frame]
  (evaluate-filters (assoc frame variable (make-binding variable datum))))

(defn create-empty-frame []
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


