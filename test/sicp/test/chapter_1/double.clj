(ns sicp.test.chapter-1.double
  (:use sicp.chapter-1.ex-41)
  (:use clojure.test))

(deftest double-test
  (is (=
       (((do-twice (do-twice do-twice)) inc) 5)
       21)))
