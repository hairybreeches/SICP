(ns sicp.chapter-3.circuits)

;wires
(defn get-signal
  [wire])

(defn set-signal!
  [wire])

(defn add-action!
  [wire action])

;primitive gates
;inverter
(defn- logical-not
  [signal]
  (cond (= signal 0) 1
        (= signal 1) 0))

(defn inverter-delay 2)


(defn- invert-output
  [input output]
  (let [new-value (logical-not (get-signal input))]
    (after-delay
     inverter-delay
     (partial set-signal! output new-value))))


(defn inverter
  [input output]
  (add-action!
   input
   (partial invert-output input output)))

;and

(defn and-gate-delay 3)

(defn- and-action
  [a1 a2 output]
  (let [new-value (logical-and
                   (get-signal a1)
                   (get-signal a2))]
    (after-delay
      and-gate-delay
      (partial set-signal! output new-value))))


(defn and-gate
  [a1 a2 output]
  (let [action (partial and-action a1 a2 ouput)]
    (add-action! a1 action)
    (add-action! a2 action)))


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
