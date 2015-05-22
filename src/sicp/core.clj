(ns sicp.core
  (:use sicp.1-35))

(defn function [x]
  (/ (java.lang.Math/log 1000) (java.lang.Math/log x)))


(defn -main
  "steve"
  []
  (fixed-point function 2 0.00001)
  (prn)
  (fixed-point #(/ (+ (function %) %) 2) 2 0.0001))

