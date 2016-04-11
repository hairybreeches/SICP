(ns sicp.test.chapter-4.sequences
  (:use sicp.chapter-4.interpreter.repl)
  (:use sicp.chapter-4.interpreter.lazy-lists)
  (:use sicp.chapter-4.sequences)
  (:use sicp.chapter-4.interpreter.laziness)
  (:use sicp.chapter-4.interpreter.default-environment)
  (:use clojure.test))

(defn list-evals-to [expected & statements]
  (is (= expected
         (lazy-list->list (apply execute statements)))))

(deftest can-exclude
  (list-evals-to '(1 3 5 7 9)
            null?
            filter-code
            member?
            exclude-code
            '(exclude '(2 4 6 8) '(1 2 3 4 5 6 7 8 9))))

(deftest can-map
  (list-evals-to '(2 4 6 8 10)
            null?
            map-code
            '(map (lambda (x) (* 2 x)) '(1 2 3 4 5))))

(deftest lazy-list-from-quotation
  (list-evals-to '(1 2 3) ''(1 2 3)))

(deftest car-on-lazy-list
  (is (= (execute '(car '(1 2 3))) 1)))

(deftest cdr-on-lazy-list
  (list-evals-to '(2 3) '(cdr '(1 2 3))))

(deftest cons-on-lazy-list
  (list-evals-to '(0 1 2 3) '(cons 0 '(1 2 3))))

(deftest cons-with-variable
  (list-evals-to '(0 1 2 3) '(let ((x 0)) (cons x '(1 2 3)))))

(deftest printing-infinite-sequences
  (let [env (create-new-environment)]
    (actual-value
      '(define (natural-numbers n) (cons n (natural-numbers (+ n 1))))
      env)
    (is (= (str (actual-value '(natural-numbers 0) env)) "(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ...)"))))


(deftest printing-lazy-list
  (is (= (str (execute ''(1 2 3))) "(1 2 3)")))
