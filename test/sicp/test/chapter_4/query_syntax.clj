(ns sicp.test.chapter-4.query-syntax
  (:use sicp.chapter-4.logic.query-syntax)
  (:use clojure.test))

(deftest can-expand-question-mark
  (is (= (expand-question-mark '?x) '(? x))))
