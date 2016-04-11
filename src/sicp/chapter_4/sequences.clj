(ns sicp.chapter-4.sequences)

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

(def null?
  '(define (null? l)
     (= 'null l)))
