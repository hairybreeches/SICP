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
        action-procedures (ref '())

        set-my-signal!
        (fn [new-value]
            (if (not (= new-value @signal-value))
                (dosync (ref-set signal-value new-value)
                        (call-each @action-procedures))))

        accept-action-procedure!
        (fn [proc]
          (alter action-procedures cons proc)
          (proc))

        dispatch
        (fn [m]
            (cond (= m :get-signal) @signal-value
                  (= m :set-signal!) set-my-signal!
                  (= m :add-action!) accept-action-procedure!))]

    dispatch))
  ([] (make-wire 0)))

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

(defn inverter-delay 2)

(defn inverter
  [input output]
  (make-box output logical-not inverter-delay input))

;and
(defn and-gate-delay 3)

(defn- logical-and
  [a1 a2]
  (cond (= 1 a1 a2) 1
        :else 0))

(defn and-gate
  [in1 in2 output]
  (make-box output logical-and and-gate-delay in1 in2))

;or
(defn or-gate-delay 5)

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
  (let [bigendian-ins1 (reverse ins1)
        bigendian-ins2 (reverse ins2)
        bigendian-outs (reverse outs)]

    (loop [ins1 bigendian-ins1
         ins2 bigendian-ins2
         outs bigendian-outs
         carry carry]
      (if (every? empty? ins1 ins2 outs)
          nil
          (let [new-carry (make-wire)]
               (full-adder (first ins1) (first ins2) new-carry (first outs) carry)
               (recur (rest ins1) (rest ins2) (rest outs) new-carry))))))












