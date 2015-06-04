(ns sicp.prime-test
  (:use clojure.test)
  (:use sicp.chapter-1.ex-21)
  (:use sicp.chapter-1.ex-23))

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

(deftest fermat-prime
  (test-prime #(fermat-prime? % 30)))

(deftest miller-rabin-prime
  (test-prime #(miller-rabin-prime? % 30)))


(deftest miller-rabin-not-fooled-by-carmichael-numbers
  (let [prime-predicate? #(miller-rabin-prime? % 30)]
    (is (not (prime-predicate? 561)))
    (is (not (prime-predicate? 1105)))
    (is (not (prime-predicate? 1729)))
    (is (not (prime-predicate? 2465)))
    (is (not (prime-predicate? 2821)))
    (is (not (prime-predicate? 6601)))))
