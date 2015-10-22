(ns sicp.chapter-3.assignment
  (:use sicp.chapter-2.plane-geometry))


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

(defn call-the-police
  []
  (fn[& _] "Police called"))

(defn make-account
  [initial-balance password]
  (let [balance (ref initial-balance)
        incorrect-attempts (ref 0)]

    (defn withdraw
        [amount]
        (dosync
          (if (>= @balance amount)
              (do (alter balance - amount)
                  @balance)
              "Insufficient funds")))

    (defn deposit
      [amount]
      (dosync
       (alter balance + amount)
       @balance))

    (defn dispatch
      [m]
      (cond (= m :withdraw) withdraw
            (= m :deposit) deposit
            :else (throw (Exception. (str "Unknown Request " m " in make-account")))))


    (defn password-dispatch
      [password-attempt & args]
      (if (= password password-attempt)
          (dosync
             (ref-set incorrect-attempts 0)
              (apply dispatch args))
          (dosync
             (alter incorrect-attempts inc)
             (if (< @incorrect-attempts 7)
                 (fn [& _] "Incorrect password")
                 (call-the-police)))))


    password-dispatch))

(defn monte-carlo
  [trials experiment]
   (loop [trials-remaining trials
          trials-passed 0]
     (cond (= trials-remaining 0) (/ trials-passed trials)
           (experiment) (recur (dec trials-remaining) (inc trials-passed))
           :else (recur (dec trials-remaining) trials-passed))))

(defn random-in-range
  [low high]
  (let [diff (- high low)]
    (+ low (rand diff))))

(defn point-in-box
  [{x-min :x-min x-max :x-max y-min :y-min y-max :y-max}]
  (make-point
    (random-in-range x-min x-max)
    (random-in-range y-min y-max)))

(defn box-area
  [{x-min :x-min x-max :x-max y-min :y-min y-max :y-max}]
  (*
    (- x-max x-min)
    (- y-max y-min )))


(defn monte-carlo-integration
  [border-box
   predicate
   number-of-tests]

  (let [integration-test (fn [] (predicate (point-in-box border-box)))
        proportion-of-border-box (monte-carlo number-of-tests integration-test)]
      (* proportion-of-border-box (box-area border-box))))































