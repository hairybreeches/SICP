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
  (:use sicp.chapter-4.while)
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
    '((+ 2 3) quote)
    ))

(deftest can-branch-true
  (evals-to 4
    '(true 4 3 if)
    ))

(deftest numbers-are-true
  (evals-to 4
    '(2 4 3 if)
    ))

(deftest can-branch-false
  (evals-to 3
    '(false 4 3 if)
   ))

(deftest can-branch-false-with-no-alternative
  (evals-to false
    '(false 4 if)
            ))

(deftest can-cond-to-first
  (evals-to 1
    '((true 1)
       (false 4 3)
      cond)
                  ))

(deftest can-cond-to-second
  (evals-to 4
    '((false 1)
       (3 4)
       (4 2)
        cond)
                  ))

(deftest can-cond-to-last
  (evals-to 8
    '((false 1)
       (false 4)
       (else 8)
      cond)
                  ))


(deftest can-use-functional-cond
  (evals-to 7
    '((false 1)
       (7 => ([x] x lambda))
       (else 8)
       cond)
                  ))

(deftest can-evaluate-lambda
  (evals-to 4
    '((() 4 lambda))
            ))

(deftest can-receive-parameters-in-lambda
  (evals-to 12
    '(12 ((x) x lambda))
            ))

(deftest can-define-values
  (evals-to 8
    '((x 8 define)
       x
      begin)
            ))

(deftest can-set-values
  (evals-to 12
    '((x 8 define)
       (x 12 set!)
       x
      begin)
            ))

(deftest and-evaluates-to-true
  (evals-to 4
    '((true
          true
          4
          "4"
           and)
         4
         5
      if)
            ))


(deftest and-evaluates-to-false
  (evals-to 5
    '((true
          false
          ("this will throw an exception if evaluated")
          "4"
           and)
         4
         5
      if)
            ))

(deftest or-evaluates-to-false
  (evals-to 5
    '(( false
              false
              false
           or)
         4
         5
      if)
            ))


(deftest or-evaluates-to-true
  (evals-to 4
    '((false
              true
              ("this will throw an exception if evaluated")
              "4"
           or)
         4
         5
      if )
            ))

(deftest let-defines-values
  (evals-to 4
            '(((x 3) (y 4))
               y
              let)))

(deftest let-values-override
  (evals-to 4
            '(
               (z 2 define)
               (((z 4))
                  z
                let)
               begin)
            ))

(deftest let-values-go-out-of-scope
  (evals-to 2
            '(
               (z 2 define)
               (((z 4))
                  false
                let )
               z
               begin)
            ))

(deftest let*-passes-values-through
  (evals-to 3
            '(((x 3) (y x) (z y))
                   z
              let*)
            ))

(deftest can-evaluate-plus
  (evals-to 2
            '(1 1 +)
            ))

(deftest define-function-form
  (evals-to 16
            '(
              ((add-3-to-total a b)
                      (a b 3 +)
               define)
              (4 9 add-3-to-total)
               begin)
            ))

(deftest lambda-resolved-at-correct-time
  (evals-to 7
            '(
               ((add-steve a)
                       (steve a + )
                define)
               (steve 4 define )
               (3 add-steve )
               begin)
            ))

(deftest named-let-no-recursion
  (evals-to 4
    '(unused-name
          ((z 4))
          z
       let)))

(deftest named-let
  (evals-to 13
    '(
      ((fib n)
        (fib-iter
          ((a 1)
           (b 0)
           (count n))
          ((count 0 =)
              b
              ((a b +) a (count 1 -) fib-iter)
           if)
          let)
       define)

       (7 fib)
       begin)
            ))

(deftest while-iterations
  (evals-to 7
            '((a 3 define)
               ((a 7 <)
                (a (a 1 +) set!)
                while)
               a
              begin)))


