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





