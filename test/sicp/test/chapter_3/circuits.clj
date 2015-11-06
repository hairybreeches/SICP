(ns sicp.test.chapter-3.circuits
  (:use clojure.test)
  (:use sicp.chapter-3.circuits))


(deftest an-inverter-inverts-its-initial-current-when-its-zero
  (let [wire-in (make-wire 0)
        wire-out (make-wire)
        inverter (inverter wire-in wire-out)]
    (is (= (get-signal wire-out) 0))
    (increment-time!)
    (is (= (get-signal wire-out) 0))
    (increment-time!)
    (is (= (get-signal wire-out) 1))))
