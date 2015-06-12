(ns sicp.test.carmichael
  (:use clojure.test)
  (:use sicp.chapter-1.ex-27))

(deftest carmichael-success
	(is (carmichael? 561))
  (is (carmichael? 1105))
  (is (carmichael? 1729))
  (is (carmichael? 2465))
  (is (carmichael? 2821))
  (is (carmichael? 6601)))

(deftest carmichael-failure
	(is (not (carmichael? 562)))
  (is (not (carmichael? 1107)))
  (is (not (carmichael? 1735)))
  (is (not (carmichael? 2469)))
  (is (not (carmichael? 2825))))


(deftest primes-arent-carmichael
	(is (not (carmichael? 701)))
  (is (not (carmichael? 2467))))





