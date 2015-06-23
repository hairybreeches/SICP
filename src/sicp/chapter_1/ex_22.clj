(ns sicp.chapter-1.ex-22
  (:use sicp.chapter-1.ex-23))

(defn timed-prime-test [n]
  (print "\n")
  (print n)
  (time (fast-prime? n 1000)))


(defn show-prime-complexity []
  (timed-prime-test 1000000007)
  (timed-prime-test 1000000009)
  (timed-prime-test 1000000021)
  (timed-prime-test 10000000019)
  (timed-prime-test 10000000033)
  (timed-prime-test 10000000061)
  (timed-prime-test 100000000003)
  (timed-prime-test 100000000019)
  (timed-prime-test 100000000057)
  (timed-prime-test 1000000000039)
  (timed-prime-test 1000000000061)
  (timed-prime-test 1000000000063))
