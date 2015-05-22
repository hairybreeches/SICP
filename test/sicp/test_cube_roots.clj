(ns sicp.test-cube-roots
  (:use sicp.1-40)
  (:use clojure.test)
  (:use sicp.test-accuracy))

(deftest integer-root
  (is-roughly=
   (newtons-method (cubic -6.34 13.36 -9.36) 1.5 0.0001)
   2
   4))

(deftest non-integer-root
  (is-roughly=
   (newtons-method (cubic -6.34 13.36 -9.36) 3 0.0001)
   2.34
   4))

