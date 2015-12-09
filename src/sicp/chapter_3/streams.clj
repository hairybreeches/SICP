(ns sicp.chapter-3.streams
  (:use sicp.average)
  (:use clojure.math.numeric-tower)
  (:use sicp.chapter-1.ex-16)
  (:use sicp.average))

(defn add-seqs
  [& args]
  (apply map + args))

(def integers-seq
  (drop 1 (range)))

(def powers-of-two
  (iterate #(* % 2) 1))

(defn mul-seqs
  [& args]
  (apply map * args))

(def factorials-inner
  (iterate
   (fn [[fac n]]
     (let
       [new (inc n)]
          [(* fac new) new]))
   [1 1]
   ))

(def factorials
  (map first factorials-inner))

(defn partial-sums
  [s]
  (reductions + s))


(defn sq-merge-weighted-pair
  [weight stream1 stream2]
  (lazy-seq
  (cond
    (empty? stream1) stream2
    (empty? stream2) stream1
    :else (let [s1car (first stream1)
                s2car (first stream2)]
            (cond (< (weight s1car) (weight s2car)) (cons s1car (sq-merge-weighted-pair weight (rest stream1) stream2))
                  :else (cons s2car (sq-merge-weighted-pair weight (rest stream2) stream1)))))))

(defn sq-merge-weighted
  [weight & streams]
  (reduce (partial sq-merge-weighted-pair weight) streams))

(defn scale
  [n sequ]
  (map #(* n %) sequ))

(defn seq-merge-pair
  [seq1 seq2]
  (cond
    (empty? seq1) seq2
    (empty? seq2) seq1
    :else (lazy-seq
           (let [s1head (first seq1)
                 s2head (first seq2)]
            (cond (< s1head s2head) (cons s1head (seq-merge-pair (rest seq1) seq2))
                  (> s1head s2head) (cons s2head (seq-merge-pair (rest seq2) seq1))
                  :else (cons s1head (seq-merge-pair (rest seq1) (rest seq2))))))))

(defn seq-merge
  [& seqs]
  (reduce seq-merge-pair seqs))

(defn hamming
  ([] (hamming [1]))
  ([waiting]
   (lazy-seq
      (cons
         (first waiting)
         (hamming
          (seq-merge
           (rest waiting)
           (map #(* % (first waiting)) [2 3 5])))))))

(def hamming-numbers (hamming))

(defn expand
  [numer denom radix]
  (lazy-seq
    (cons
     (quot (* numer radix) denom)
     (expand (rem (* numer radix) denom) denom radix))))

(defn div-seqs
  [numer denom]
  (mul-seqs numer
            (map #(/ 1 %) denom)))

(defn integrate-series
  [series]
  (div-seqs series
            integers-seq))

(def cosine-series)

(def sine-series
  (lazy-seq
    (cons 0 (integrate-series cosine-series))))

(def cosine-series
  (lazy-seq
    (cons 1 (scale -1 (integrate-series sine-series)))))

(defn mul-series
  [s1 s2]
  (lazy-seq
  (cons
    (* (first s1)
       (first s2))
    (add-seqs
       (scale (first s1) (rest s2))
       (scale (first s2) (rest s1))
       (cons 0 (mul-series (rest s1) (rest s2)))))))

(defn- invert-series-with-constant-term-one
  [series]
  (let [result (ref false)]
    (dosync (ref-set result
    (lazy-seq
      (cons
       1
      (scale
          -1
          (mul-series
                (rest series)
                @result)))))
            @result)))

(defn invert-series
  [series]
  (let [scaling-factor (first series)]
  (if (= scaling-factor 0)
      (throw (Exception. "Cannot divide by a stream with zero constant term"))
      (scale
         (/ 1 scaling-factor)
         (invert-series-with-constant-term-one series)))))

(defn div-series
  [numer denom]
  (mul-series numer (invert-series denom)))

(def tan-series
  (div-series sine-series cosine-series))

(defn stream-limit
  [stream tolerance]
  (loop [last-value (first stream)
         stream (rest stream)]
    (if (< (abs (- last-value (first stream)))
           tolerance)
        (first stream)
        (recur (first stream)
               (rest stream)))))

(defn sqrt-improve
  [guess x]
  (average guess (/ x guess)))

(defn sqrt-stream
  [x]
  (iterate #(sqrt-improve % x) 1.0))

(defn sqrt-tolerance
  [x tolerance]
  (stream-limit (sqrt-stream x) tolerance))

(defn euler-transform
  [s]
  (let [s0 (first s)
        s1 (second s)
        s2 (nth s 2)]
    (lazy-seq
      (cons (- s2 (/ (square (- s2 s1))
                     (+ s0 (* -2 s1) s2)))
            (euler-transform (rest s))))))

(defn make-tableau
  [transform s]
  (iterate transform s))

(defn accelerated-sequence
  [transform s]
  (map first (make-tableau transform s)))

(defn get-log2-term
  [n]
  (let [absolute (/ 1 n)]
    (if (is-even? n)
        (* -1 absolute)
        absolute)))

(def log2-approximations
  (partial-sums (map get-log2-term integers-seq)))

(def accelerated-log2-approximations
  (euler-transform log2-approximations))

(def super-accelerated-log2-approximations
  (accelerated-sequence euler-transform log2-approximations))

(defn get-log2
  [tolerance]
  (stream-limit super-accelerated-log2-approximations tolerance))

(defn pairs
  [s t weight]
  (lazy-seq (cons [(first s) (first t)]
        (sq-merge-weighted
         weight
         (map (fn [x] [(first s) x]) (rest t))
         (pairs (rest s) (rest t) weight)))))

(defn all-pairs
  [s t]
  (lazy-seq
  (cons [(first s) (first t)]
        (sq-merge-weighted
         (fn [[a b]] (+ a b))
         (map (fn [x] [(first s) x]) (rest t))
         (map (fn [x] [x (first t)]) (rest s))
         (all-pairs (rest s) (rest t))))))


(def integer-pairs
  (pairs integers-seq integers-seq second))

(defn all-triples-with-highest-term
  [n]
  (map (fn [[i j]] [i j n]) (take-while #(<= (second %) n) integer-pairs)))

(defn triples
  [s t u]
  (mapcat all-triples-with-highest-term integers-seq))


(defn find-consecutives
  [n function sq]
  (lazy-seq
  (loop [sq sq]
    (let [first-value (function (first sq))
          equal-to-current? (fn [n] (= (function n) first-value))
          same-values (take-while equal-to-current? sq)
          number-same (count same-values)
          remaining (drop-while equal-to-current? sq)]
      (if (>= number-same n)
          (cons {:output first-value :inputs same-values} (find-consecutives n function remaining))
          (recur remaining))))))

(defn integral
  [delayed-integrand initial-value dt]
  (cons
     initial-value
    (lazy-seq
      (let [integrand @delayed-integrand]
        (if (empty? integrand)
            '()
            (integral (delay (rest integrand))
                      (+ (* dt (first integrand))
                         initial-value)
                      dt))))))

(defn RC
  [R C dt]
  (fn [current v0]
    (add-seqs
     (scale R current)
     (->
      (scale (/ 1 C) current)
      (delay)
      (integral v0 dt)))))

(defn sign-change-detector
  [v0 v1]
  (cond (< v0 0 v1) 1
        (> v0 0 v1) -1
        :else 0))

(defn zero-crossings
  [sense-data]
  (map sign-change-detector sense-data (rest sense-data)))

(defn smooth
  [stream]
  (lazy-seq
  (cons
    (first stream)
    (map average stream (rest stream)))))

(defn smoothed-zero-crossings
  ([input-stream]
   (-> input-stream
       (smooth)
       (zero-crossings))))

(defn solve
  [f y0 dt]
  (let [dy (ref false)
        y (integral dy y0 dt)]
    (dosync
      (ref-set
       dy
       (lazy-seq (map f y)))
     y)))

(defn solve-2nd
  [y0 dy0 dt f]
  (let [dy2 (ref false)
        dy (integral dy2 dy0 dt)
        y (integral (delay dy) y0 dt)]
    (dosync
      (ref-set
       dy2
       (lazy-seq (map f y dy)))
     y)))

(defn solve-2nd-linear
  [a b y0 dy0 dt]
  (->>
   (fn [y dy] (+ (* a dy) (* b y)))
   (solve-2nd y0 dy0 dt)))























