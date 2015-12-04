(ns sicp.chapter-3.streams
  (:use sicp.average)
  (:use clojure.math.numeric-tower)
  (:use sicp.chapter-1.ex-16)
  (:use sicp.average))

(defn memo-proc
  [proc]
  (let [run (ref false)
        value (ref false)]
    (fn []
      (if (not @run)
          (dosync
            (ref-set value (proc))
            (ref-set run true)))
      @value)))



(defn force-stream
  [value]
  (value))

(defmacro delay-stream
  [form]
  `(memo-proc (fn [] ~form)))

(defmacro stream-cons
  [car-form cdr-form]
  `(let [cdr# (delay-stream ~cdr-form)
         car# ~car-form]
     (fn [m#]
         (cond (= m# :car)
               car#

               (= m# :cdr)
               cdr#))))

(defn stream-car
  [stream]
  (stream :car))

(defn stream-cdr
  [stream]
  (force-stream (stream :cdr)))

(defn empty-stream?
  [stream]
  (= (stream-car stream) :empty-stream))

(def empty-stream
  (stream-cons :empty-stream (throw (Exception. "Cannot find the cdr of the empty stream"))))

(defn stream->list
  [stream]
  (if (empty-stream? stream)
      `()
       (cons (stream-car stream)
             (stream->list (stream-cdr stream)))))

(defn stream-enumerate-interval
  [low high]
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (inc low) high))))

(defn stream-map
  [proc & argstreams]
    (if (some empty-stream? argstreams)
        empty-stream
        (stream-cons
           (apply proc (map stream-car argstreams))
           (apply stream-map proc (map stream-cdr argstreams)))))

(defn stream-ref
  [s n]
  (loop
    [s s
     n n]
  (if (= n 0)
      (stream-car s)
      (recur (stream-cdr s)
             (dec n)))))

(defn stream-filter
  [pred stream]
  (loop
    [stream stream]
  (cond

   (empty-stream? stream)
   empty-stream

   (pred (stream-car stream))
   (stream-cons (stream-car stream)
                (stream-filter pred
                               (stream-cdr stream)))

   :else
   (recur (stream-cdr stream)))))

(defn stream-take
  [n stream]
  (reverse
    (loop [n n
           result '()
           stream stream]

      (if (= n 0)
          result
          (recur
            (dec n)
            (cons (stream-car stream)
                  result)
           (stream-cdr stream))))))

(defn add-streams
  [& args]
  (apply stream-map + args))

(defn add-seqs
  [& args]
  (apply map + args))

(def ones
  (stream-cons 1 ones))

(def integers
  (stream-cons 1 (add-streams ones integers)))

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


(defn partial-sums-stream
  [stream]
  (let [partial-sum (ref false)]
    (dosync
      (ref-set partial-sum
               (stream-cons
                      (stream-car stream)
                      (add-streams
                        (stream-cdr stream)
                        @partial-sum))))
    @partial-sum))

(defn stream-merge-pair
  [stream1 stream2]
  (cond
    (empty-stream? stream1) stream2
    (empty-stream? stream2) stream1
    :else (let [s1car (stream-car stream1)
                s2car (stream-car stream2)]
            (cond (< s1car s2car) (stream-cons s1car (stream-merge-pair (stream-cdr stream1) stream2))
                  (> s1car s2car) (stream-cons s2car (stream-merge-pair (stream-cdr stream2) stream1))
                  :else (stream-cons s1car (stream-merge-pair (stream-cdr stream1) (stream-cdr stream2)))))))


(defn stream-merge
  [& streams]
  (reduce stream-merge-pair streams))

(defn stream-merge-weighted-pair
  [weight stream1 stream2]
  (cond
    (empty-stream? stream1) stream2
    (empty-stream? stream2) stream1
    :else (let [s1car (stream-car stream1)
                s2car (stream-car stream2)]
            (cond (< (weight s1car) (weight s2car)) (stream-cons s1car (stream-merge-weighted-pair weight (stream-cdr stream1) stream2))
                  :else (stream-cons s2car (stream-merge-weighted-pair weight (stream-cdr stream2) stream1))))))

(defn stream-merge-weighted
  [weight & streams]
  (reduce (partial stream-merge-weighted-pair weight) streams))



(defn scale-stream
  [n stream]
  (stream-map #(* n %) stream))

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
  ([series]
    (lazy-seq
      (cons
       1
      (scale
          -1
          (mul-series
                (rest series)
                (invert-series-with-constant-term-one series)))))))

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

(defn list->stream
  [l]
  (if
    (empty? l)
    empty-stream
    (stream-cons (first l) (list->stream (rest l)))))

(defn stream-limit
  [stream tolerance]
  (loop [last-value (stream-car stream)
         stream (stream-cdr stream)]
    (if (< (abs (- last-value (stream-car stream)))
           tolerance)
        (stream-car stream)
        (recur (stream-car stream)
               (stream-cdr stream)))))

(defn sqrt-improve
  [guess x]
  (average guess (/ x guess)))

(defn sqrt-stream
  [x]
  (let [guesses (ref false)]
    (dosync
     (ref-set guesses
              (stream-cons 1.0
                           (stream-map #(sqrt-improve % x) guesses))))))

(defn sqrt-tolerance
  [x tolerance]
  (stream-limit (sqrt-stream x) tolerance))

(defn euler-transform
  [s]
  (let [s0 (stream-ref s 0)
        s1 (stream-ref s 1)
        s2 (stream-ref s 2)]
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(defn make-tableau
  [transform s]
  (stream-cons s (make-tableau transform (transform s))))

(defn accelerated-sequence
  [transform s]
  (stream-map stream-car (make-tableau transform s)))

(defn get-log2-term
  [n]
  (let [absolute (/ 1 n)]
    (if (is-even? n)
        (* -1 absolute)
        absolute)))

(def log2-approximations
  (partial-sums-stream (stream-map get-log2-term integers)))

(def accelerated-log2-approximations
  (euler-transform log2-approximations))

(def super-accelerated-log2-approximations
  (accelerated-sequence euler-transform log2-approximations))

(defn get-log2
  [tolerance]
  (stream-limit super-accelerated-log2-approximations tolerance))

(defn stream-interleave-pair
  [s t]
  (if (empty-stream? s)
      t
      (stream-cons (stream-car s)
                   (stream-interleave-pair t (stream-cdr s)))))

(defn stream-interleave
  [& args]
  (reduce stream-interleave-pair args))

(defn pairs
  [s t weight]
  (stream-cons [(stream-car s) (stream-car t)]
        (stream-merge-weighted
         weight
         (stream-map (fn [x] [(stream-car s) x]) (stream-cdr t))
         (pairs (stream-cdr s) (stream-cdr t) weight))))

(defn all-pairs
  [s t]
  (stream-cons [(stream-car s) (stream-car t)]
        (stream-merge-weighted
         (fn [[a b]] (+ a b))
         (stream-map (fn [x] [(stream-car s) x]) (stream-cdr t))
         (stream-map (fn [x] [x (stream-car t)]) (stream-cdr s))
         (all-pairs (stream-cdr s) (stream-cdr t)))))

(defn stream-take-while
  [predicate stream]
  (if (predicate (stream-car stream))
      (stream-cons (stream-car stream) (stream-take-while predicate (stream-cdr stream)))
      empty-stream))

(defn stream-drop-while
  [predicate stream]
  (loop [stream stream]
    (if (not (predicate (stream-car stream)))
        stream
        (recur (stream-cdr stream)))))

(defn stream-concat
  [streams]
  (cond
   (empty-stream? streams) empty-stream
   (empty-stream? (stream-car streams)) (stream-concat (stream-cdr streams))
   :else (stream-cons (stream-car (stream-car streams)) (stream-concat (stream-cons (stream-cdr (stream-car streams)) (stream-cdr streams))))))


(def integer-pairs
  (pairs integers integers second))

(defn all-triples-with-highest-term
  [n]
  (stream-map (fn [[i j]] [i j n]) (stream-take-while #(<= (second %) n) integer-pairs)))

(defn triples
  [s t u]
  (stream-concat (stream-map all-triples-with-highest-term integers)))


(defn find-consecutives
  [n function stream]
  (loop [stream stream]
    (let [first-value (function (stream-car stream))
          equal-to-current? (fn [n] (= (function n) first-value))
          same-values (stream->list (stream-take-while equal-to-current? stream))
          number-same (count same-values)]
      (if (>= number-same n)
          (stream-cons {:output first-value :inputs same-values} (find-consecutives n function (stream-drop-while equal-to-current? stream)))
          (recur (stream-drop-while equal-to-current? stream))))))

(defn integral
  [delayed-integrand initial-value dt]
  (stream-cons
   initial-value
   (let [integrand (force-stream delayed-integrand)]
     (if (empty-stream? integrand)
         empty-stream
         (integral (delay-stream (stream-cdr integrand))
                   (+ (* dt (stream-car integrand))
                      initial-value)
                   dt)))))

(defn RC
  [R C dt]
  (fn [current v0]
    (add-streams
     (scale-stream R current)
     (->
      (scale-stream (/ 1 C) current)
      (delay-stream)
      (integral v0 dt)))))

(defn sign-change-detector
  [v0 v1]
  (cond (< v0 0 v1) 1
        (> v0 0 v1) -1
        :else 0))

(defn zero-crossings
  [sense-data]
  (stream-map sign-change-detector sense-data (stream-cdr sense-data)))

(defn smooth
  [stream]
  (stream-cons
    (stream-car stream)
    (stream-map average stream (stream-cdr stream))))

(defn smoothed-zero-crossings
  ([input-stream]
   (-> input-stream
       (smooth)
       (zero-crossings))))

(defn solve
  [f y0 dt]
  (let [dy (ref false)
        y (integral (delay-stream @dy) y0 dt)]
    (dosync
      (ref-set
       dy
       (stream-map f y))
     y)))

(defn solve-2nd
  [y0 dy0 dt f]
  (let [dy2 (ref false)
        dy (integral (delay-stream @dy2) dy0 dt)
        y (integral (delay-stream dy) y0 dt)]
    (dosync
      (ref-set
       dy2
       (stream-map f y dy))
     y)))

(defn solve-2nd-linear
  [a b y0 dy0 dt]
  (->>
   (fn [y dy] (+ (* a dy) (* b y)))
   (solve-2nd y0 dy0 dt)))























