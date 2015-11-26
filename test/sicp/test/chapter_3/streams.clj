(ns sicp.test.chapter-3.streams
  (:use sicp.chapter-3.streams)
  (:use clojure.test))

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
  (is (= '(1 2 4 8 16 32) (stream-take 6 powers-of-two))))

(deftest stream-factorials
  (is (= '(1 2 6 24 120 720) (stream-take 6 factorials))))

(deftest can-find-partial-sums
  (is (= '(1 3 6 10 15 21) (stream-take 6 (partial-sums integers)))))

(deftest hamming-numbers
  (is (= '(1 2 3 4 5 6 8 9 10 12 15 16 18 20) (stream-take 14 hamming))))

(deftest division-expansions
  (is (= '(1 4 2 8 5 7 1 4) (stream-take 8 (expand 1 7 10))))
  (is (= '(3 7 5 0 0 0 0 0) (stream-take 8 (expand 3 8 10)))))

(deftest cos-squared-plus-sin-squared
  (is (= '(1 0 0 0 0 0 0 0) (stream-take 8
                                         (add-streams (mul-series cosine-series cosine-series)
                                                      (mul-series sine-series sine-series))))))

(deftest sinx-roughly-x-for-smallx
  (is (= '(0 1) (stream-take 2 sine-series))))


(deftest can-generate-tan-series
  (is (= '() (stream-take 12 tan-series))))

(deftest can-generate-inverse
  (is (= '() (stream-take 5 (invert-series-with-constant-term-one (list->series '(-1 1)))))))






