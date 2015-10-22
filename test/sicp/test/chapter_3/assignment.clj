(ns sicp.test.chapter-3.assignment
  (:use clojure.test)
  (:use sicp.chapter-3.assignment))

(deftest can-accumulate
  (let [accumulator (make-accumulator 12)]
    (do (accumulator 13)
        (accumulator 14))
    (is (= (accumulator 11) 50))))


(deftest can-monitor-calls
  (let [monitored-square (make-monitored (fn[x] (* x x)))]
    (is (= (monitored-square 2) 4))
    (is (= (monitored-square 6) 36))
    (is (= (monitored-square -9) 81))
    (is (= (monitored-square :how-many-calls) 3))
    (monitored-square :reset-count)
    (is (= (monitored-square :how-many-calls) 0))))
