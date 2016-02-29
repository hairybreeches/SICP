(ns sicp.test.chapter-4.lazy-lists
  (:use sicp.chapter-4.interpreter.repl)
  (:use sicp.chapter-4.interpreter.lazy-lists)
  (:use clojure.test))

(deftest lazy-list-from-quotation
  (is (= (lazy-list->list (execute ''(1 2 3))) '(1 2 3))))

(deftest car-on-lazy-list
  (is (= (execute '(car '(1 2 3))) 1)))

(deftest cdr-on-lazy-list
  (is (= (lazy-list->list (execute '(cdr '(1 2 3)))) '(2 3))))

(deftest cons-on-lazy-list
  (is (= (lazy-list->list (execute '(cons 0 '(1 2 3)))) '(0 1 2 3))))

(deftest cons-with-variable
  (is (= (lazy-list->list (execute '(let ((x 0)) (cons x '(1 2 3))))) '(0 1 2 3))))

(deftest printing-lazy-list
  (is (= (str (execute ''(1 2 3))) "(1 2 3)")))
