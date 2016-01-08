(ns sicp.test.chapter-4.environments
  (:use sicp.chapter-4.environments)
  (:use clojure.test))

(deftest can-retrieve-value-in-extended-environment
  (is (= 4
         (lookup-variable-value
           'x
           (extend-environment '(x) '(4) the-empty-environment)))))
