(ns sicp.chapter-3.assignment)


(defn make-accumulator
  [initial-value]
  (let [accumulate-value (ref initial-value)]
    (fn
      [to-add]
      (dosync
        (alter accumulate-value + to-add)
        @accumulate-value))))

(defn make-monitored
  [to-monitor]
  (let [number-of-calls (ref 0)]
    (fn
      [& args]
      (cond
         (= (first args) :how-many-calls) @number-of-calls
         (= (first args) :reset-count) (dosync (ref-set number-of-calls 0))
         :else (dosync (alter number-of-calls inc)
                       (apply to-monitor args))))))


