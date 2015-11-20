(ns sicp.test.chapter-3.concurrency
  (:use sicp.chapter-3.concurrency)
  (:use clojure.test))


(defn release-after [failure-count release-after]
  (let [failures-since-last-release (ref 0)]
    (fn [mutex]
      (dosync
       (alter failure-count inc)
       (alter failures-since-last-release inc)
       (if (>= @failures-since-last-release release-after)
           (do
             (ref-set failures-since-last-release 0)
             (mutex :release)))))))


(deftest unacquired-mutex-acquired-straight-away
  (let [failure-count (ref 0)
        on-failure (release-after failure-count 5)
        mutex (make-mutex on-failure)]
    (mutex :acquire)
    (is (= @failure-count 0))))

(deftest acquired-mutex-blocks-until-released
  (let [failure-count (ref 0)
        on-failure (release-after failure-count 5)
        mutex (make-mutex on-failure)]
    (mutex :acquire)
    (mutex :acquire)
    (is (= @failure-count 5))))

(deftest released-mutex-can-be-acquired
  (let [failure-count (ref 0)
        on-failure (release-after failure-count 5)
        mutex (make-mutex on-failure)]
    (mutex :acquire)
    (mutex :release)
    (mutex :acquire)
    (is (= @failure-count 0))))

(defn unacquired-semaphore-can-be-acquired-n-times
  [make-semaphore]
  (let [failure-count (ref 0)
        on-failure (release-after failure-count 3)
        semaphore (make-semaphore 4 on-failure)]

    (semaphore :acquire)
    (semaphore :acquire)
    (semaphore :acquire)
    (semaphore :acquire)
    (is (= @failure-count 0))))

(defn fully-acquired-semaphore-blocks
  [make-semaphore]
  (let [failure-count (ref 0)
        on-failure (release-after failure-count 3)
        semaphore (make-semaphore 4 on-failure)]

    (semaphore :acquire)
    (semaphore :acquire)
    (semaphore :acquire)
    (semaphore :acquire)
    (semaphore :acquire)
    (is (= @failure-count 3))))

(defn restored-semaphore-doesnt-block
  [make-semaphore]
  (let [failure-count (ref 0)
        on-failure (release-after failure-count 3)
        semaphore (make-semaphore 4 on-failure)]

    (semaphore :acquire)
    (semaphore :acquire)
    (semaphore :acquire)
    (semaphore :acquire)
    (semaphore :release)
    (semaphore :acquire)
    (is (= @failure-count 0))))

(deftest unacquired-count-semaphore-can-be-acquired-n-times
  (unacquired-semaphore-can-be-acquired-n-times make-semaphore-count))

(deftest fully-acquired-count-semaphore-blocks
  (fully-acquired-semaphore-blocks make-semaphore-count))

(deftest restored-count-semaphore-doesnt-block
  (restored-semaphore-doesnt-block make-semaphore-count))

(deftest unacquired-mutex-semaphore-can-be-acquired-n-times
  (unacquired-semaphore-can-be-acquired-n-times make-semaphore-mutex))

(deftest fully-acquired-mutex-semaphore-blocks
  (fully-acquired-semaphore-blocks make-semaphore-mutex))

(deftest restored-mutex-semaphore-doesnt-block
  (restored-semaphore-doesnt-block make-semaphore-mutex))

(deftest can-swap-balances-with-serialized-exchange
  (let [account1 (make-account 40)
        account2 (make-account 20)]
    (serialized-exchange account1 account2)
    (is (= (account1 :balance) 20))
    (is (= (account2 :balance) 40))))

(deftest can-swap-balances-with-serialized-exchange-accounts-reversed
  (let [account1 (make-account 10)
        account2 (make-account 25)]
    (serialized-exchange account2 account1)
    (is (= (account1 :balance) 25))
    (is (= (account2 :balance) 10))))










