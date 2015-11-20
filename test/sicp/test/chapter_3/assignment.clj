(ns sicp.test.chapter-3.assignment
  (:use clojure.test)
  (:use sicp.chapter-3.assignment)
  (:use sicp.chapter-1.ex-16)
  (:use sicp.chapter-2.plane-geometry)
  (:use sicp.test.assertions))

(deftest can-accumulate
  (let [accumulator (make-accumulator 12)]
    (do (accumulator 13)
        (accumulator 14))
    (is (= (accumulator 11) 50))))


(deftest can-monitor-calls
  (let [monitored-square (make-monitored (fn[x] (* x x)))]
    (is (= (monitored-square 2) 4))
    (is (= (monitored-square 6) 36))
    (is (= (monitored-square -9) 81))
    (is (= (monitored-square :how-many-calls) 3))
    (monitored-square :reset-count)
    (is (= (monitored-square :how-many-calls) 0))))


(deftest can-make-account
  (let [account (make-account 250 :password1)]
      (is (= ((account :password1 :deposit) 20) 270))
      (is (= ((account :password1 :withdraw) 30) 240))
      (is (= ((account :password1 :withdraw) 300) "Insufficient funds"))
      (is (= ((account :password2 :withdraw) 12) "Incorrect password"))
      (is (= ((account :password1 :deposit) 500) 740))))


(deftest getting-password-wrong-seven-times-calls-police
  (let [account (make-account 250 :password1)]
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Police called"))))


(deftest correct-password-resets-count
  (let [account (make-account 250 :password1)]
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password1 :withdraw) 20) 230))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Incorrect password"))
      (is (= ((account :password2 :withdraw) 20) "Police called"))))

(defn circle-of-radius-3-predicate
  [point]
  (<= (+
        (square (- (x-point point) 5))
        (square (- (y-point point) 7)))
      9))

(def box-containing-circle
  {:x-min 2
   :x-max 8
   :y-min 4
   :y-max 10})


(deftest monte-carlo-integration-circle
  (is-roughly= (monte-carlo-integration box-containing-circle
                                        circle-of-radius-3-predicate
                                        100000)
               (* 9 java.lang.Math/PI)
               1))


(deftest can-generate-not-very-random-numbers
  (is (= (rand-seeded :generate) 1))
  (is (= (rand-seeded :generate) 2))
  (is (= (rand-seeded :generate) 3))
  (is (= (rand-seeded :generate) 4))
  (is (= ((rand-seeded :reset) 91) 91))
  (is (= (rand-seeded :generate) 92))
  (is (= (rand-seeded :generate) 93)))

(deftest joint-account-shares-balance
  (let [original (make-account 30 :original-password)
        joint-account (make-joint original :original-password :joint-password)]

    (is (= ((original :original-password :withdraw) 10) 20))
    (is (= ((joint-account :joint-password :withdraw) 20) 0))))


(deftest cannot-access-accounts-with-other-password
  (let [original (make-account 1000 :original-password)
        joint-account (make-joint original :joint-password :joint-password)]

    (is (= ((original :joint-password :withdraw) 10) "Incorrect password"))
    (is (= ((joint-account :original-password :withdraw) 10) "Incorrect password"))))

(def spooky-state (ref 0))

(defn f
  [n]
  (dosync
    (let [last-value @spooky-state]
      (ref-set spooky-state n)
      last-value)))

(deftest evaluation-order-matters-left-to-right
  (dosync (ref-set spooky-state 0))
  (let [a (f 0)
        b (f 1)]
    (is (= (+ a b) 0))))

(deftest evaluation-order-matters-right-to-left
  (dosync (ref-set spooky-state 0))
  (let [b (f 1)
        a (f 0)]
    (is (= (+ a b) 1))))

























