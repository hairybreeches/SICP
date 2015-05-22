(ns sicp.test-double
  (:use sicp.1-41)
  (:use clojure.test))

(deftest double-test
  (is (=
       (((do-twice (do-twice do-twice)) inc) 5)
       21)))
