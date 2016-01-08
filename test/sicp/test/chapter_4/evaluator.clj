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
       (definition x 8)
       x)
            ))

(deftest can-set-values
  (evals-to 12
    '(begin
       (definition x 8)
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


