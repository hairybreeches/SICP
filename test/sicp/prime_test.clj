(ns sicp.prime-test
  (:use clojure.test)
  (:use sicp.1-21)
  (:use sicp.1-23))

(defn prime-success [prime-predicate?]
	(is (prime-predicate? 991))
  (is (prime-predicate? 997))
  (is (prime-predicate? 1009))
  (is (prime-predicate? 1013)))

(defn prime-failure [prime-predicate?]
	(is (not (prime-predicate? 992)))
  (is (not (prime-predicate? 993)))
  (is (not (prime-predicate? 1011)))
  (is (not (prime-predicate? 1017))))

(defn test-prime [prime-predicate?]
  (prime-success prime-predicate?)
  (prime-failure prime-predicate?))

(deftest slow-prime
  (test-prime prime?))

(deftest fast-prime
  (test-prime #(fast-prime? % 30)))



