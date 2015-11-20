(ns sicp.test.chapter-3.streams
  (:use sicp.chapter-3.streams)
  (:use clojure.test))

(deftest can-evaluate-car-of-cons
  (is
   (=
    (car-stream (cons-stream 2 :empty))
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
