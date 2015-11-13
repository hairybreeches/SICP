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
