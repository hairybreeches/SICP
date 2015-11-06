(ns sicp.chapter-3.circuits)

;wires
(defn get-signal
  [wire])

(defn set-signal!
  [wire])

(defn add-action!
  [wire action])

;primitive gates
(defn- make-update
  [output operation wait & inputs]
  (let [new-value (apply operation (map get-signal inputs))]
    (after-delay
     wait
     (partial set-signal! output new-value))))

;inverter
(defn- logical-not
  [signal]
  (cond (= signal 0) 1
        (= signal 1) 0))

(defn inverter-delay 2)


(defn- invert-output
  [output input]
  (make-update output logical-not inverter-delay input))


(defn inverter
  [input output]
  (add-action!
   input
   (partial invert-output output input)))

;and
(defn and-gate-delay 3)

(defn- logical-and
  [a1 a2]
  (cond (= 1 a1 a2) 1
        :else 0))

(defn- and-action
  [output in1 in2]
  (make-update output logical-and and-gate-delay in1 in2))


(defn and-gate
  [in1 in2 output]
  (let [action (partial and-action output in1 in2)]
    (add-action! in1 action)
    (add-action! in2 action)))

;or
(defn or-gate-delay 5)

(defn logical-or
  [o1 o2]
  (cond (= 0 o1 o2) 0
        :else 1))

(defn- or-action
  [output in1 in2]
  (make-update output logical-or or-gate-delay in1 in2))

(defn or-gate
  [in1 in2 output]
  (let [action (partial or-action output in1 in2)]
    (add-action! in1 action)
    (add-action! in2 action)))



;circuits
(defn half-adder
  [a b s c]
  (let [d (make-wire)
        e (make-wire)]

    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))

(defn full-adder
      [a b c-in sum c-out]
      (let [s (make-wire)
            c1 (make-wire)
            c2 (make-wire)]
        (half-adder b c-in s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 c-out)))
