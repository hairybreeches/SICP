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


(defn make-account
  [initial-balance password]
  (let [balance (ref initial-balance)]

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
          (apply dispatch args)
          (fn [& _] "Incorrect password")))


    password-dispatch))





