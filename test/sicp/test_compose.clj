(ns sicp.test-compose
  (:use sicp.1-42)
  (:use clojure.test))

(deftest test-compose
  (is (= ((compose #(* % %) inc) 6) 49)))

(deftest square-twice
  (is (= ((repeated #(* % %) 2) 5) 625)))

(deftest square-three-times
  (is (= ((repeated #(* % %) 3) 2) 256)))
