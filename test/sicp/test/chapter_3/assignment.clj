(ns sicp.test.chapter-3.assignment
  (:use clojure.test)
  (:use sicp.chapter-3.assignment))

(deftest can-accumulate
  (let [accumulator (make-accumulator 12)]
    (do (accumulator 13)
        (accumulator 14))
    (is (= (accumulator 11) 50))))
