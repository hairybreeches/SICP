(ns sicp.test.chapter-3.assignment
  (:use clojure.test)
  (:use sicp.chapter-3.assignment))

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
