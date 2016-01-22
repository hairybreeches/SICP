(ns sicp.test.chapter-4.evaluator
  (:use sicp.chapter-4.evaluator)
  (:use sicp.chapter-4.if)
  (:use sicp.chapter-4.cond)
  (:use sicp.chapter-4.begin)
  (:use sicp.chapter-4.assignment)
  (:use sicp.chapter-4.lambda)
  (:use sicp.chapter-4.quote)
  (:use sicp.chapter-4.definition)
  (:use sicp.chapter-4.application)
  (:use sicp.chapter-4.primitive-datatypes)
  (:use sicp.chapter-4.variables)
  (:use sicp.chapter-4.let)
  (:use sicp.chapter-4.boolean-operators)
  (:use clojure.test))

(defn evals-to
  [result code]
  (is (= (execute code) result)))

(deftest can-evaluate-numbers
  (evals-to 4
    '4
    ))

(deftest can-evaluate-strings
  (evals-to "steve"
    '"steve"
    ))

(deftest can-quote
  (evals-to '(+ 2 3)
    '(quote (+ 2 3))
    ))

(deftest can-branch-true
  (evals-to 4
    '(if true 4 3)
    ))

(deftest numbers-are-true
  (evals-to 4
    '(if 2 4 3)
    ))

(deftest can-branch-false
  (evals-to 3
    '(if false 4 3)
   ))

(deftest can-branch-false-with-no-alternative
  (evals-to false
    '(if false 4)
            ))

(deftest can-cond-to-first
  (evals-to 1
    '(cond
       (true 1)
       (false 4 3))
                  ))

(deftest can-cond-to-second
  (evals-to 4
    '(cond
       (false 1)
       (3 4)
       (4 2))
                  ))

(deftest can-cond-to-last
  (evals-to 8
    '(cond
       (false 1)
       (false 4)
       (else 8))
                  ))


(deftest can-use-functional-cond
  (evals-to 7
    '(cond
       (false 1)
       (7 => (lambda [x] x))
       (else 8))
                  ))

(deftest can-evaluate-lambda
  (evals-to 4
    '((lambda () 4))
            ))

(deftest can-receive-parameters-in-lambda
  (evals-to 12
    '((lambda (x) x) 12)
            ))

(deftest can-define-values
  (evals-to 8
    '(begin
       (define x 8)
       x)
            ))

(deftest can-set-values
  (evals-to 12
    '(begin
       (define x 8)
       (set! x 12)
       x)
            ))

(deftest and-evaluates-to-true
  (evals-to 4
    '(if (and true
              true
              4
              "4")
         4
         5)
            ))


(deftest and-evaluates-to-false
  (evals-to 5
    '(if (and true
              false
              ("this will throw an exception if evaluated")
              "4")
         4
         5)
            ))

(deftest or-evaluates-to-false
  (evals-to 5
    '(if (or false
              false
              false)
         4
         5)
            ))


(deftest or-evaluates-to-true
  (evals-to 4
    '(if (or false
              true
              ("this will throw an exception if evaluated")
              "4")
         4
         5)
            ))

(deftest let-defines-values
  (evals-to 4
            '(let ((x 3) (y 4))
               y)))

(deftest let-values-override
  (evals-to 4
            '(begin
               (define z 2)
               (let ((z 4))
                  z))
            ))

(deftest let-values-go-out-of-scope
  (evals-to 2
            '(begin
               (define z 2)
               (let ((z 4))
                  false)
               z)
            ))

(deftest let*-passes-values-through
  (evals-to 3
            '(let* ((x 3) (y x) (z y))
                   z)
            ))

(deftest can-evaluate-plus
  (evals-to 2
            '(+ 1 1)
            ))

(deftest define-function-form
  (evals-to 16
            '(begin
              (define (add-3-to-total a b)
                      (+ a b 3))
              (add-3-to-total 4 9))
            ))

(deftest lambda-resolved-at-correct-time
  (evals-to 7
            '(begin
               (define (add-steve a)
                       (+ steve a))
               (define steve 4)
               (add-steve 3))))

(deftest named-let-no-recursion
  (evals-to 4
    '(let unused-name
          ((z 4))
          z)))

(deftest named-let
  (evals-to 13
    '(begin
      (define
        (fib n)
        (let fib-iter
          ((a 1)
           (b 0)
           (count n))
          (if (= count 0)
              b
              (fib-iter (+ a b) a (- count 1)))))

       (fib 7))
            ))



