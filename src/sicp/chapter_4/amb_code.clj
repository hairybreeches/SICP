(ns sicp.chapter-4.amb-code
  (:use sicp.chapter-4.interpreter.repl))

(def require-code
  '(define (require p)
             (if (not p) (amb))))

(def an-integer-between
  '(define (an-integer-between low high)
             (if (> low high)
               (amb)
               (amb low (an-integer-between (+ 1 low) high)))))

(def an-integer-starting-from
  '(define (an-integer-starting-from low)
     (amb low (an-integer-starting-from (+ 1 low)))))

(def filter-code
  '(define (filter predicate things)
     (cond ((null? things) '())
           ((predicate (car things)) (cons (car things) (filter predicate (cdr things))))
           (else (filter predicate (cdr things))))))

(def map-code
  '(define (map transformer things)
     (if
       (null? things)
       '()
       (cons (transformer (car things)) (map transformer (cdr things))))))

(def an-element-of
  '(define (an-element-of things)
     (require (not (null? things)))
     (amb (car things) (an-element-of (cdr things)))))

(def member?
  '(define (member? collection object)
     (cond ((null? collection) false)
           ((= (car collection) object) true)
           (else (member? (cdr collection) object)))))

(def exclude-code
  '(define (exclude excluded objects)
     (filter
       (lambda
         (object)
         (not (member? excluded object)))
       objects)))

(def a-pythagorean-triple-between
  '(define (a-pythagorean-triple-between low high)
     (let ((high-squared (* high high)))
       (let ((i (an-integer-between low high)))
         (let ((i-squared (* i i)))
           (let ((j-max (sqrt (- high-squared i-squared))))
             (let ((j (an-integer-between i j-max)))
               (let ((k (sqrt (+ i-squared (* j j)))))
                 (require (integer? k))
                 (list i j k)))))))))

(def a-pythagorean-triple
  '(define (a-pythagorean-triple)
     (let ((root2 (sqrt 2)))
       (let ((hypotenuse (an-integer-starting-from 5)))
         (let ((hypotenuse-squared (* hypotenuse hypotenuse))
               (j-max (- hypotenuse 1))
               (j-min (+ 1 (int (/ hypotenuse root2)))))
           (let ((j (an-integer-between j-min j-max)))
             (let ((i (sqrt (- hypotenuse-squared (* j j)))))
               (require (integer? i))
               (list i j hypotenuse))))))))
