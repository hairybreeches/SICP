(ns sicp.test.chapter-3.propagation_of_constraints
  (:use sicp.chapter-3.propagation_of_constraints)
  (:use clojure.test))



(deftest celsius-to-fahrenheit-conversion
  (let
    [C (make-connector)
     F (make-connector)
     converter (celsius-to-fahrenheit-converter C F)]

    (set-value! C 25 :user)
    (is (= (get-value F) 77))
    (forget-value! C :user)
    (set-value! F 212 :user)
    (is (= (get-value C) 100))))


(deftest averager-test
  (let [a (make-connector)
        b (make-connector)
        average (make-connector)]
    (averager a b average)

    (set-value! a 2 :user)
    (set-value! b 4 :user)
    (is (= (get-value average) 3))
    (forget-value! a :user)
    (set-value! average 6 :user)
    (is (= (get-value a) 8))))


