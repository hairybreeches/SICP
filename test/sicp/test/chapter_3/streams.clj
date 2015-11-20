(ns sicp.test.chapter-3.streams
  (:use sicp.chapter-3.streams)
  (:use clojure.test))

(deftest can-evaluate-car-of-cons
  (is
   (=
    (car-stream (cons-stream 2 :empty-stream))
    2)))

(deftest can-evaluate-cdr-of-cons
  (is
   (=
    5
    (cdr-stream (cons-stream 2 (+ 2 3))))))

(deftest cdr-not-evaluated-on-stream-creation
  (is
   (=
    (car-stream (cons-stream 2 (throw (Exception. "This code shouldn't be evaluated"))))
    2)))


(deftest stream-map-test
  (is
   (=
     '(1 3 5 7 9)
     (->> (stream-map + (stream-enumerate-interval 0 4) (stream-enumerate-interval 1 5))
          stream->list))))
