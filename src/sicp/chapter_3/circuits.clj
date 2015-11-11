(ns sicp.chapter-3.circuits)

;wires
(defn get-signal
  [wire]
  (wire :get-signal))

(defn set-signal!
  [wire new-value]
  ((wire :set-signal!) new-value))

(defn add-action!
  [wire action]
  ((wire :add-action!) action))

(defn- call-each
  [procedures]
  (doseq
    [procedure procedures]
    (procedure)))

(defn make-wire
  ([initial-value]
  (let [signal-value (ref initial-value)
        action-procedures (ref [])

        set-my-signal!
        (fn [new-value]
            (if (not (= new-value @signal-value))
                (dosync (ref-set signal-value new-value)
                        (call-each @action-procedures))))

        accept-action-procedure!
        (fn [proc]
          (dosync
            (alter action-procedures #(cons proc %))
            (proc)))

        dispatch
        (fn [m]
            (cond (= m :get-signal) @signal-value
                  (= m :set-signal!) set-my-signal!
                  (= m :add-action!) accept-action-procedure!))]

    dispatch))
  ([] (make-wire 0)))

;the agenda
(defn make-agenda
  []
  (let [now (ref 0)
        actions (ref {})

        increment-time!
        (fn []
          (dosync
           (alter now inc)
           (call-each (@actions @now))))

        add-action!
        (fn [wait action]
          (dosync
            (alter actions (partial merge-with concat) {(+ @now wait) [action]})))

        dispatch
        (fn [m]
          (cond (= m :add-action!) add-action!
                (= m :increment-time!) (increment-time!)))]
    dispatch))

(def agenda (make-agenda))

(defn increment-time! []
  (agenda :increment-time!))

(defn after-delay
  [wait action]
  ((agenda :add-action!) wait action))

;primitive gates
(defn- update
  [output operation wait inputs]
  (let [new-value (apply operation (map get-signal inputs))]
    (after-delay
     wait
     (partial set-signal! output new-value))))

(defn- make-box
  [output operation wait & inputs]
  (let [action (partial update output operation wait inputs)]
    (doseq
      [input inputs]
      (add-action! input action))))

;inverter
(defn- logical-not
  [signal]
  (cond (= signal 0) 1
        (= signal 1) 0))

(def inverter-delay 2)

(defn inverter
  [input output]
  (make-box output logical-not inverter-delay input))

;and
(def and-gate-delay 3)

(defn- logical-and
  [a1 a2]
  (cond (= 1 a1 a2) 1
        :else 0))

(defn and-gate
  [in1 in2 output]
  (make-box output logical-and and-gate-delay in1 in2))

;or
(def or-gate-delay 5)

(defn logical-or
  [o1 o2]
  (cond (= 0 o1 o2) 0
        :else 1))

(defn or-gate
  [in1 in2 output]
  (make-box output logical-or or-gate-delay in1 in2))

;circuits
(defn composite-or
  [in1 in2 output]
  (let [a (make-wire)
        b (make-wire)
        c (make-wire)]
    (inverter in1 a)
    (inverter in2 b)
    (and-gate a b c)
    (inverter c output)))

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

(defn ripple-adder
  [ins1 ins2 outs carry]
    (loop [ins1 ins1
         ins2 ins2
         outs outs
         carry-out carry]
      (if (every? empty? [ins1 ins2 outs])
          nil
          (let [carry-in (make-wire)]
               (full-adder (first ins1) (first ins2) carry-in (first outs) carry-out)
               (recur (rest ins1) (rest ins2) (rest outs) carry-in)))))












