(ns sicp.chapter-3.concurrency)

(defn- clear!
  [cell]
  (dosync
   (if @cell

      (do
        (ref-set cell false)
        true)

      false)))

(defn- test-and-set!
  [cell]
  (dosync
   (if (ensure cell)
        true
       (dosync
         (ref-set cell true)
        false))))



(defn make-mutex
  [on-failure]
  (let [cell (ref false)]
    (letfn [
      (the-mutex
        [m]
          (cond (= m :acquire)
                (if (test-and-set! cell)
                    (do
                      (on-failure the-mutex)
                      (the-mutex :acquire)))

                (= m :release)
                (clear! cell)

                (= m :try-acquire)
                (not (test-and-set! cell))))]
    the-mutex)))


(defn- test-and-set-number!
  [n max-n]
  (dosync
   (if (< (ensure n) max-n)
       (do
         (alter n inc)
         false)
       true)))

(defn- release!
  [n]
  (dosync
   (if (> @n 0)
     (alter n dec)
     (throw (Exception. "Can't release unacquired semaphore!")))))

(defn make-semaphore-count
  [max-n on-failure]
  (let [cell (ref 0)]
    (letfn
      [(the-semaphore
        [m]
          (cond (= m :acquire)
                (if (test-and-set-number! cell max-n)
                    (do
                      (on-failure the-semaphore)
                      (the-semaphore :acquire)))

                (= m :release)
                (release! cell)))]
    the-semaphore)))


(defn- acquire-a-mutex
  [mutexes]
  (loop [mutexes mutexes]
    (cond (empty? mutexes)
          true

          ((first mutexes) :try-acquire)
          false

          :else
          (recur (rest mutexes)))))

(defn- release-a-mutex
  [mutexes]
  (loop [mutexes mutexes]
    (cond (empty? mutexes)
          (throw (Exception. "No acquired mutexes!"))

          (not ((first mutexes) :release))
          (recur (rest mutexes)))))

;interestingly, everyone else viewed this problem as "use a mutex to block access to the count of locks taken"
;this kinda makes more sense, but still.
(defn make-semaphore-mutex
  [max-n on-failure]
  (let [mutexes (->>(iterate (fn [_] (make-mutex (fn[mutex]))) 0)
                    (drop 1)
                    (take max-n))]
    (letfn[
       (the-semaphore
        [m]
          (cond (= m :acquire)
                (if (acquire-a-mutex mutexes)
                    (do
                      (on-failure the-semaphore)
                      (the-semaphore :acquire)))

                (= m :release)
                (release-a-mutex mutexes)))]
    the-semaphore)))


(defn make-serializer
  []
  (let [mutex (make-mutex (fn [_] ))]
    (fn [p]
       (fn [& args]
          (mutex :acquire)
          (let [value (apply p args)]
                   (mutex :release)
                   value)))))

(defn exchange
  [account1 account2]
  (let [difference (- (account1 :balance)
                      (account2 :balance))]
    ((account1 :withdraw) difference)
    ((account2 :deposit) difference)))

(def account-index (ref 0))

(defn generate-account-index
  []
  (dosync
    (alter account-index inc)))


(defn make-account
  [initial-balance]
  (let [balance (ref initial-balance)
        account-index (generate-account-index)
        withdraw
        (fn [amount]
          (if (>= @balance amount)
              (dosync
               (alter balance #(- % amount)))
              (throw (Exception. "insufficient funds"))))

        deposit
        (fn [amount]
          (dosync
               (alter balance #(+ % amount))))

        serializer (make-serializer)]


        (fn [m]
          (cond
           (= m :withdraw) withdraw
           (= m :deposit) deposit
           (= m :serializer) serializer
           (= m :index) account-index
           (= m :balance) @balance))))

 (defn serialized-exchange
  [account1 account2]
  (if (> (account2 :index)
         (account1 :index))
      (serialized-exchange account2 account1)
      (let [serializer1 (account1 :serializer)
            serializer2 (account2 :serializer)]
        ((serializer1 (serializer2 exchange))
         account1
         account2))))
























