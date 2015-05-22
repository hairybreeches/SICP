(ns sicp.test-compose
  (:use sicp.1-42)
  (:use clojure.test))

(deftest test-compose
  (is (= ((compose #(* % %) inc) 6) 49)))
