(ns sicp.test.chapter-4.lazy-lists
  (:use sicp.chapter-4.interpreter.repl)
  (:use sicp.chapter-4.interpreter.lazy-lists)
  (:use sicp.chapter-4.interpreter.laziness)
  (:use sicp.chapter-4.interpreter.default-environment)
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

(deftest printing-infinite-sequences
  (let [env (create-new-environment)]
    (actual-value
      '(define (natural-numbers n) (cons n (natural-numbers (+ n 1))))
      env)
    (is (= (str (actual-value '(natural-numbers 0) env)) "(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ...)"))))


(deftest printing-lazy-list
  (is (= (str (execute ''(1 2 3))) "(1 2 3)")))
