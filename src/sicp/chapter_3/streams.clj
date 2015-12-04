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

(def ones
  (stream-cons 1 ones))

(def integers
  (stream-cons 1 (add-streams ones integers)))


(def powers-of-two
  (stream-cons 1 (add-streams powers-of-two powers-of-two)))

(defn mul-streams
  [& args]
  (apply stream-map * args))

(def factorials
  (stream-cons 1 (mul-streams factorials (stream-cdr integers))))

(defn partial-sums
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

(def hamming
  (stream-cons 1 (stream-merge
                   (scale-stream 2 hamming)
                   (scale-stream 3 hamming)
                   (scale-stream 5 hamming))))

(defn expand
  [numer denom radix]
  (stream-cons
   (quot (* numer radix) denom)
   (expand (rem (* numer radix) denom) denom radix)))

(defn div-streams
  [numer denom]
  (mul-streams numer
               (stream-map #(/ 1 %) denom)))

(defn integrate-series
  [series]
  (div-streams series
               integers))

(def cosine-series)

(def sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(def cosine-series
  (stream-cons 1 (scale-stream -1 (integrate-series sine-series))))

(defn mul-series
  [s1 s2]
  (stream-cons
    (* (stream-car s1)
       (stream-car s2))
    (add-streams
       (scale-stream (stream-car s1) (stream-cdr s2))
       (scale-stream (stream-car s2) (stream-cdr s1))
       (stream-cons 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

(defn- invert-series-with-constant-term-one
  [series]
  (let [inverse (ref false)]
    (dosync
      (ref-set inverse
        (stream-cons
         1
         (scale-stream
              -1
              (mul-series
                   (stream-cdr series)
                    @inverse)))))
      @inverse))

(defn invert-series
  [series]
  (let [scaling-factor (stream-car series)]
  (if (= scaling-factor 0)
      (throw (Exception. "Cannot divide by a stream with zero constant term"))
      (scale-stream
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

(def zeroes
  (stream-cons 0 zeroes))

(defn list->series
  [l]
  (if (empty? l)
      zeroes
      (stream-cons (first l)
                   (list->series (rest l)))))

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
  (partial-sums (stream-map get-log2-term integers)))

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
  [integrand initial-value dt]
  (let [result (ref false)]
    (dosync
     (ref-set result (stream-cons
                      initial-value
                      (add-streams (scale-stream dt integrand)
                                   @result)))
     @result)))

(defn RC
  [R C dt]
  (fn [current v0]
    (add-streams
     (scale-stream R current)
     (->
      (scale-stream (/ 1 C) current)
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




















