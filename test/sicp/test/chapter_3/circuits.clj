(ns sicp.test.chapter-3.circuits
  (:use clojure.test)
  (:use sicp.chapter-3.circuits))


(defn check-value-for
  [increments wire expected-value]
  (doseq [i (range increments)]
    (increment-time!)
    (is (= (get-signal wire) expected-value))))

(defn wait
  [wait]
  (doseq [i (range wait)]
    (increment-time!)))

(defn test-primitive-component
  [wire-out update-delay eventual-value]

  ;the base state of the wire is 0
  (is (= (get-signal wire-out) 0))

  ;for one less than the update delay, the wire doesn't update
  (check-value-for (dec update-delay) wire-out 0)

  ;after the update delay, the component has worked, and the signal should be updated
  (increment-time!)
  (is (= (get-signal wire-out) eventual-value)))

(defn test-inverter
  [input-value eventual-value]
  (let [wire-in (make-wire input-value)
        wire-out (make-wire)
        inverter (inverter wire-in wire-out)]
    (test-primitive-component wire-out 2 eventual-value)))

(deftest an-inverter-inverts-its-initial-current-when-its-zero
  (test-inverter 1 0))

(deftest an-inverter-inverts-its-initial-current-when-its-one
  (test-inverter 0 1))

(defn test-and
  [in1 in2 out]
  (let [wire1-in (make-wire in1)
        wire2-in (make-wire in2)
        wire-out (make-wire)
        and-box (and-gate wire1-in wire2-in wire-out)]

    (test-primitive-component wire-out 3 out)))

(deftest test-and-box
  (test-and 0 0 0)
  (test-and 0 1 0)
  (test-and 1 0 0)
  (test-and 1 1 1))


(defn test-or
  [in1 in2 out]
  (let [wire1-in (make-wire in1)
        wire2-in (make-wire in2)
        wire-out (make-wire)
        or-box (or-gate wire1-in wire2-in wire-out)]

    (test-primitive-component wire-out 5 out)))

(deftest test-or-box
  (test-or 0 0 0)
  (test-or 0 1 1)
  (test-or 1 0 1)
  (test-or 1 1 1))

(defn test-composite-or
  [in1 in2 out]
  (let [wire1-in (make-wire in1)
        wire2-in (make-wire in2)
        wire-out (make-wire)
        composite-or-box (or-gate wire1-in wire2-in wire-out)]
    (wait 7)
    (is (= (get-signal wire-out) out))))

(deftest test-composite-or-box
  (test-composite-or 0 0 0)
  (test-composite-or 0 1 1)
  (test-composite-or 1 0 1)
  (test-composite-or 1 1 1))

(defn run-half-adder
  [in1
   in2]
  (let [[wire1-in wire2-in] (map make-wire [in1 in2])
        wire-out (make-wire)
        wire-carry-out (make-wire)
        adder (half-adder wire1-in wire2-in wire-out wire-carry-out)]
    (wait 8)
    (map get-signal [wire-carry-out wire-out])))

(deftest test-half-adder
  (is (= [0 0] (run-half-adder 0 0)))
  (is (= [0 1] (run-half-adder 0 1)))
  (is (= [0 1] (run-half-adder 1 0)))
  (is (= [1 0] (run-half-adder 1 1))))

(defn run-full-adder
  [in1
   in2
   carry-in]
  (let [[wire1-in wire2-in wire-carry-in] (map make-wire [in1 in2 carry-in])
        wire-out (make-wire)
        wire-carry-out (make-wire)
        adder (full-adder wire1-in wire2-in wire-carry-in wire-out wire-carry-out)]
    (wait 14)
    (map get-signal [wire-carry-out wire-out])))

(deftest test-full-adder
  (is (= [1 1] (run-full-adder 1 1 1)))
  (is (= [1 0] (run-full-adder 1 1 0)))
  (is (= [1 0] (run-full-adder 1 0 1)))
  (is (= [1 0] (run-full-adder 0 1 1)))
  (is (= [0 1] (run-full-adder 0 0 1)))
  (is (= [0 1] (run-full-adder 0 1 0)))
  (is (= [0 1] (run-full-adder 1 0 0)))
  (is (= [0 0] (run-full-adder 0 0 0))))


(defn run-ripple-adder
  [digits-in1
   digits-in2]
  (let [wires-in1 (map make-wire digits-in1)
        wires-in2 (map make-wire digits-in2)
        wires-out (map (fn [_] (make-wire)) digits-in1)
        carry-wire (make-wire)
        adder (ripple-adder wires-in1 wires-in2 wires-out carry-wire)
        wait-time (+ (* (count wires-in1) (+ (* 3 3) 2 5)) 2 3 5)]
    (wait wait-time)
    (map
      get-signal
      (cons carry-wire wires-out))))

(deftest test-ripple-adder-7-and-5
  (is
    (=
      (run-ripple-adder
       [1 1 1]
       [1 0 1])
      [1 1 0 0])))







