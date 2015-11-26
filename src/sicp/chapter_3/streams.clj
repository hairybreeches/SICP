(ns sicp.chapter-3.streams)

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
    (if (empty-stream? (first argstreams))
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
    (mul-series
     (scale-stream (stream-car s1) (stream-cdr s2))
     (scale-stream (stream-car s2) (stream-cdr s1)))))

(defn invert-series-with-constant-term-one
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
  (if (= (stream-car series) 0)
      (throw (Exception. "Cannot divide by a stream with zero constant term"))
      (scale-stream
         (/ 1 (stream-car series))
         (invert-series-with-constant-term-one series))))

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











