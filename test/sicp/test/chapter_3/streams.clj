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






