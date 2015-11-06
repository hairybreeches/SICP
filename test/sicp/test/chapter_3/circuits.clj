(ns sicp.test.chapter-3.circuits
  (:use clojure.test)
  (:use sicp.chapter-3.circuits))


(defn test-inverter
  [input-value ultimate-output]
  (let [wire-in (make-wire input-value)
        wire-out (make-wire)
        inverter (inverter wire-in wire-out)]

    ;the base state of the wire is 0
    (is (= (get-signal wire-out) 0))

    (increment-time!)
    ;after one time, the inverter has not worked yet (it has a delay of two)
    (is (= (get-signal wire-out) 0))

    (increment-time!)
    ;after two time, the inverter has worked, and the signal should be the inverse of the input
    (is (= (get-signal wire-out) ultimate-output))))



(deftest an-inverter-inverts-its-initial-current-when-its-zero
  (test-inverter 1 0))

(deftest an-inverter-inverts-its-initial-current-when-its-one
  (test-inverter 0 1))
