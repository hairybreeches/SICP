(ns sicp.test.chapter-2.sequences
  (:use sicp.chapter-2.sequences)
  (:use clojure.test))


(deftest last-item-in-single-element-sequence
  (is (= :a (last-pair [:a]))))

(deftest last-item-in-several-element-sequence
  (is (= 6 (last-pair '(1 2 3 4 5 6)))))

(deftest reverse-empty-sequence
  (is (empty? (reverse-impl '()))))

(deftest reverse-single-element-sequence
  (is (= '(:zi) (reverse-impl '(:zi)))))

(deftest reverse-several-element-sequence
  (is (= '(6 5 4 3 2 1) (reverse-impl '(1 2 3 4 5 6)))))

(deftest count-change-book-example
  (is (= 292 (count-change 100 us-coins))))

(deftest same-parity-odd
  (is (= '(1 3 5 7) (same-parity 1 2 3 4 5 6 7))))

(deftest same-parity-even
  (is (= '(2 4 6) (same-parity 2 3 4 5 6 7))))

(defn test-square-list[function]
  (is (= '(1 4 9 16) (function '(1 2 3 4)))))

(deftest test-square-list-primitives
  (test-square-list square-list-primitives))

(deftest test-square-list-map
  (test-square-list square-list-map))

(deftest test-for-each
  (is (= (with-out-str (for-each prn '(1 2 :a "woo!"))) "1\r\n2\r\n:a\r\n\"woo!\"\r\n")))

