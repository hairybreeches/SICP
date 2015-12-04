(ns sicp.test.chapter-3.streams
  (:use sicp.chapter-3.streams)
  (:use clojure.test)
  (:use sicp.chapter-1.ex-16)
  (:use sicp.test.assertions)
  (:use clojure.math.numeric-tower)
  (:use sicp.chapter-1.primes))

(deftest can-evaluate-car-of-cons
  (is
   (=
    (stream-car (stream-cons 2 :empty-stream))
    2)))

(deftest can-evaluate-cdr-of-cons
  (is
   (=
    5
    (stream-cdr (stream-cons 2 (+ 2 3))))))

(deftest cdr-not-evaluated-on-stream-creation
  (is
   (=
    (stream-car (stream-cons 2 (throw (Exception. "This code shouldn't be evaluated"))))
    2)))


(deftest stream-map-test
  (is
   (=
     '(1 3 5 7 9)
     (->> (stream-map + (stream-enumerate-interval 0 4) (stream-enumerate-interval 1 5))
          stream->list))))

(defn show
  [x]
  (print x)
  x)

(deftest evaluation-part-one
  (let [x (ref false)]
    (is (=
         "0"
         (with-out-str
            (dosync
              (ref-set
                x
                (stream-map show (stream-enumerate-interval 0 10)))))))


    (is (=
         "12345"
         (with-out-str
            (stream-ref @x 5))))

    (is (=
         "67"
         (with-out-str
            (stream-ref @x 7))))))

(deftest evaluation-part-two
  (let [sum (ref 0)

        accum
        (fn [x]
          (dosync
           (alter sum + x))
          x)]

    (is (= @sum 0))

    (let [sequ (stream-map accum (stream-enumerate-interval 1 20))]
      (is (= @sum 1))

      (let [y (stream-filter even? sequ)]
        (is (= @sum 3))

        (let [z (stream-filter #(= 0 (mod % 5)) sequ)]
          (is (= @sum 15))

          (stream-ref y 7)

          (is (= @sum 136))

          (stream->list z)

          (is (= @sum 210)))))))

(deftest stream-powers-of-two
  (is (= '(1 2 4 8 16 32) (take 6 powers-of-two))))

(deftest stream-factorials
  (is (= '(1 2 6 24 120 720) (take 6 factorials))))

(deftest can-find-partial-sums
  (is (= '(1 3 6 10 15 21) (take 6 (partial-sums integers-seq)))))

(deftest hamming-numbers-test
  (is (= '(1 2 3 4 5 6 8 9 10 12 15 16 18 20) (take 14 hamming-numbers))))

(deftest division-expansions
  (is (= '(1 4 2 8 5 7 1 4) (stream-take 8 (expand 1 7 10))))
  (is (= '(3 7 5 0 0 0 0 0) (stream-take 8 (expand 3 8 10)))))

(deftest adding-multiple-streams
  (is (= '(3 3 3 3) (stream-take 4 (add-streams ones ones ones)))))

(deftest sinx-roughly-x-for-smallx
  (is (= '(0 1 0 -1/6 0 1/120) (stream-take 6 sine-series))))

(deftest cosx-expansion
  (is (= '(1 0 -1/2 0 1/24 0) (stream-take 6 cosine-series))))

(deftest cos-squared
  (is (= '(1 0 -1 0 1/3) (stream-take 5 (mul-series cosine-series cosine-series)))))

(deftest cos-squared-plus-sin-squared
  (is (= '(1 0 0 0 0 0 0 0) (stream-take 8
                                         (add-streams (mul-series cosine-series cosine-series)
                                                      (mul-series sine-series sine-series))))))
(deftest can-generate-tan-series
  (is (= '(0 1 0 1/3 0 2/15 0 17/315 0 62/2835) (stream-take 10 tan-series))))

(deftest can-generate-inverse
  (is (= '(1 1 1 1 1) (stream-take 5 (invert-series (list->series '(1 -1)))))))

(deftest can-find-sqrt-2
  (is-roughly= (sqrt-tolerance 2 0.0001) (sqrt 2) 3))

(deftest log-converges-slowly
  (is-roughly= (stream-ref log2-approximations 4) 0.78333336 8))

(deftest euler-transform-log-converges-faster
  (is-roughly= (stream-ref accelerated-log2-approximations 4) 0.69358975 8))

(deftest tableau-log-converges-fastest
  (is-roughly= (stream-ref super-accelerated-log2-approximations 4) 0.6931472 8))

(deftest can-get-log2-reasonably-accurately
  (is-roughly= (get-log2 0.00000001) (java.lang.Math/log 2) 7))

(deftest finding-all-pairs
  (is (= (stream-take 10 (all-pairs integers integers)) '([1 1] [2 1] [1 2] [2 2] [3 1] [1 3] [3 2] [4 1] [2 3] [1 4]))))

(deftest can-get-integer-pairs
  (is (= '([1 1] [2 2] [1 2] [3 3] [1 3] [2 3] [1 4] [4 4] [2 4] [3 4] [1 5] [2 5]) (stream-take 12 integer-pairs))))

(deftest can-concat-streams
  (is (= (stream->list (stream-concat (list->stream (map list->stream ['(1 2 3) '(4 5 6 7) '(8)])))) '(1 2 3 4 5 6 7 8))))

(deftest can-combine-numbers-with-triples
  (is (= (stream->list (all-triples-with-highest-term 4)) '([1 1 4] [2 2 4] [1 2 4] [3 3 4] [1 3 4] [2 3 4] [1 4 4] [4 4 4] [2 4 4] [3 4 4]))))

(defn pythagorean?
  [[a b c]]
  (= (square c) (+ (square a) (square b))))


(deftest can-find-pythagorean-triples
  (is (= (stream-take 15 (stream-filter pythagorean? (triples integers integers integers)))
         '([3 4 5]
           [6 8 10]
           [5 12 13]
           [9 12 15]
           [8 15 17]
           [12 16 20]
           [15 20 25]
           [7 24 25]
           [10 24 26]
           [20 21 29]
           [18 24 30]
           [16 30 34]
           [21 28 35]
           [12 35 37]
           [15 36 39]))))

(deftest can-find-pairs-ordered-by-sum
  (is (= (stream-take 10 (pairs integers integers (fn[[a b]] (+ a b)))) '([1 1] [1 2] [2 2] [1 3] [2 3] [1 4] [3 3] [1 5] [2 4] [1 6]))))

(defn coprime235?
  [n]
  (not (or (divides? 2 n)
           (divides? 3 n)
           (divides? 5 n))))

(defn both-coprime235?
  [[i j]]
  (and (coprime235? i)
       (coprime235? j)))


(defn weird-ordering
  [[i j]]
  (+ (* 2 i)
     (* 3 j)
     (* 5 i j)))

(deftest silly-ordering-filtering-example
  (is (= '([1 1] [1 7] [1 11] [1 13] [1 17] [1 19] [1 23] [1 29] [1 31] [7 7] [1 37] [1 41])
          (stream-take 12 (stream-filter both-coprime235? (pairs integers integers weird-ordering))))))

(defn cube
  [n]
  (* n (square n)))

(defn sum-of-cubes
  [[i j]]
  (+ (cube i)
     (cube j)))

(deftest can-find-ramanujan-numbers
  (is (= '({:output 1729, :inputs ([1 12] [9 10])}
           {:output 4104, :inputs ([2 16] [9 15])}
           {:output 13832, :inputs ([2 24] [18 20])}
           {:output 20683, :inputs ([10 27] [19 24])})
          (stream-take 4 (find-consecutives 2 sum-of-cubes (pairs integers integers sum-of-cubes))))))

(defn sum-of-squares
  [[i j]]
  (+ (square i)
     (square j)))

(deftest sums-of-squares-three-ways
  (is (= '({:output 325, :inputs ([1 18] [6 17] [10 15])}
           {:output 425, :inputs ([5 20] [8 19] [13 16])}
           {:output 650, :inputs ([5 25] [11 23] [17 19])}
           {:output 725, :inputs ([7 26] [10 25] [14 23])})
          (stream-take 4 (find-consecutives 3 sum-of-squares (pairs integers integers sum-of-squares))))))

(def RC1
  (RC 5 1 0.5))

(deftest can-calculate-output-voltages
  (is (= '(7 12.5 8.5 19.0) (stream-take 4 (RC1 (list->stream '(1 2 1 3)) 2)))))

(deftest can-detect-sign-changes
  (is (=
       '(0 0 0 0 -1 0 0 0 0 1 0 0)
       (-> '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)
           (list->stream)
           (zero-crossings)
           (stream->list)))))

(deftest can-detect-smoothed-sign-changes
  (is (=
       '(0 0 0 0 0 -1 0 0 0 0 1 0)
       (-> '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)
           (list->stream)
           (smoothed-zero-crossings)
           (stream->list)))))

(deftest can-solve-for-e
  (is-roughly=
   (java.lang.Math/E)
   (stream-ref (solve identity 1 0.001) 1000)
   2))

(deftest can-solve-second-linear
  (is-roughly=
       (->
        (solve-2nd-linear 5 -6 2 5 0.0001)
        (stream-ref 10000))
       (+ (expt (java.lang.Math/E) 3) (expt (java.lang.Math/E) 2))
       1))






















