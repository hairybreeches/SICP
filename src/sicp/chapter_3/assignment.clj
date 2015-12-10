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

(defn- make-account-access
  [balance password]
  (let [incorrect-attempts (ref 0)

    withdraw
    (fn [amount]
        (dosync
          (if (>= @balance amount)
              (do (alter balance - amount)
                  @balance)
              "Insufficient funds")))

   deposit
   (fn [amount]
      (dosync
       (alter balance + amount)
       @balance))

    dispatch
    (fn [m]
      (cond (= m :withdraw) withdraw
            (= m :deposit) deposit
            (= m :get-ref) balance
            :else (throw (Exception. (str "Unknown Request " m " in make-account")))))


    password-dispatch
    (fn [password-attempt & args]
      (if (= password password-attempt)
          (dosync
             (ref-set incorrect-attempts 0)
              (apply dispatch args))
          (dosync
             (alter incorrect-attempts inc)
             (if (< @incorrect-attempts 7)
                 (fn [& _] "Incorrect password")
                 (call-the-police)))))]

    password-dispatch))

(defn make-account
  [initial-balance password]
  (make-account-access
    (ref initial-balance)
    password))

(defn make-joint
  [account existing-password joint-password]
  (make-account-access
     (account existing-password :get-ref)
     joint-password))



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

(defn get-randoms
  [initial-value]
  (iterate inc initial-value))

(defn give-randoms
  ([requests] (give-randoms requests (get-randoms 1)))
  ([requests last-randoms]
   (lazy-seq
    (if
      (empty? requests)
      '()
      (let [request (first requests)
            action (:action request)
            randoms (cond (= action :generate) last-randoms
                              (= action :reset) (get-randoms (:initial-value request)))]
        (cons (first randoms)
              (give-randoms (rest requests)
                            (rest randoms))))))))
































