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
