(ns sicp.test.chapter-3.propagation_of_constraints
  (:use sicp.chapter-3.propagation_of_constraints)
  (:use clojure.test))

(deftest celsius-to-fahrenheit-conversion
  (let
    [C (make-connector)
     F (celsius-to-fahrenheit-converter C)]

    (set-value! C 25 :user)
    (is (= (get-value F) 77))

    (forget-value! C :user)
    (set-value! F 212 :user)
    (is (= (get-value C) 100))))


(deftest averager-test
  (let [a (make-connector)
        b (make-connector)
        average (averager a b)]

    (set-value! a 2 :user)
    (set-value! b 4 :user)
    (is (= (get-value average) 3))

    (forget-value! a :user)
    (set-value! average 6 :user)
    (is (= (get-value a) 8))))


(deftest squarer-test
  (let [square (make-connector)
        root (make-connector)]

    (squarer root square)

    (set-value! root 3 :user)
    (is (= (get-value square) 9))

    (forget-value! root :user)
    (set-value! square 16 :user)
    (is (= (get-value root) 4))))


