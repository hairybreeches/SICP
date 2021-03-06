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
  (let [estimates (monte-carlo-integration box-containing-circle circle-of-radius-3-predicate)
        correct-answer (* 9 java.lang.Math/PI)]
    (is-roughly= (nth estimates 100000) correct-answer 0)))


(deftest can-generate-not-very-random-numbers
  (is (=
       (take 7 (give-randoms '({:action :generate}
                               {:action :generate}
                               {:action :generate}
                               {:action :generate}
                               {:action :reset :initial-value 91}
                               {:action :generate}
                               {:action :generate})))
        '(1 2 3 4 91 92 93))))

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

(defmacro evaluate-left-to-right
  [form]
  (let [func (first form)
        args (rest form)
        names (map (fn [_] (gensym)) args)
        declaration (apply vector (interleave names args))]
    `(let ~declaration (~func ~@names))))

(defmacro evaluate-right-to-left
  [form]
  (let [func (first form)
        args (rest form)
        names (map (fn [_] (gensym)) args)
        declaration (apply vector (interleave (reverse names) (reverse args)))]
    `(let ~declaration (~func ~@names))))


(deftest evaluation-order-matters-left-to-right
  (dosync (ref-set spooky-state 0))
    (is (= 0
         (evaluate-left-to-right (+ (f 0) (f 1))))))

(deftest evaluation-order-matters-right-to-left
  (dosync (ref-set spooky-state 0))
  (is (= 1
         (evaluate-right-to-left (+ (f 0) (f 1))))))

(deftest arguments-left-in-correct-order-by-evaluation-macros
  (is (= 2 (evaluate-left-to-right (/ 4 2))))
  (is (= 2 (evaluate-right-to-left (/ 4 2)))))

























