(ns sicp.prime-test
  (:use clojure.test)
  (:use sicp.1-21))

(deftest prime-success
	(is (prime? 991))
  (is (prime? 997))
  (is (prime? 1009))
  (is (prime? 1013)))

(deftest prime-failure
	(is (not (prime? 992)))
  (is (not (prime? 993)))
  (is (not (prime? 1011)))
  (is (not (prime? 1017))))
