(ns sicp.test.chapter-4.laziness
  (:use sicp.test.chapter-4.evaluator)
  (:use sicp.chapter-4.interpreter.repl)
  (:use sicp.chapter-4.interpreter.laziness)
  (:use sicp.chapter-4.interpreter.default-environment)
  (:use clojure.test))

(deftest timing-of-side-effects-eager-evaluation
  (let [env (create-new-environment)]
    (actual-value '(define count 0) env)
    (actual-value '(define (id x) (set! count (+ count 1)) x) env)
    (actual-value '(define w (id (id 10))) env)
    (is (= 2 (actual-value 'count env)));because id is eager, the inner call is forced when we evaluate the outer one.
    (is (= 10 (actual-value 'w env)))
    (is (= 2 (actual-value 'count env)))))

(deftest timing-of-side-effects-lazy
  (let [env (create-new-environment)]
    (actual-value '(define count 0) env)
    (actual-value '(define (id (x lazy-memo)) (set! count (+ count 1)) x) env)
    (actual-value '(define w (id (id 10))) env)
    (is (= 1 (actual-value 'count env))) ;outer call to id is evaluated as part of the define, it doesn't evaluate its args though.
    (is (= 10 (actual-value 'w env))) ;this forces the resolution of the second call to id
    (is (= 2 (actual-value 'count env))))) ;and therefore there are two calls in total

(deftest need-to-get-actual-value-of-operator
  (evals-to 7
            '(((lambda ((x lazy-memo)) x) (lambda ((x lazy-memo)) x)) 7)))

(deftest memoisation
  (let [env (create-new-environment)]
    (actual-value '(define count 0) env)
    (actual-value '(define (id (x lazy-memo)) (set! count (+ count 1)) x) env)
    (actual-value '(define (square x) (* x x)) env)
    (is (= 100 (actual-value '(square (id 10)) env)))
    (is (= 1 (actual-value 'count env))))) ;the argument is only evaluated once because it is memoised

(deftest no-memoisation
  (let [env (create-new-environment)]
    (actual-value '(define count 0) env)
    (actual-value '(define (id x) (set! count (+ count 1)) x) env)
    (actual-value '(define (square (x lazy)) (* x x)) env)
    (is (= 100 (actual-value '(square (id 10)) env)))
    (is (= 2 (actual-value 'count env))))) ;the argument is evaluated twice because it is not memoised

;the side effect is evaluated either way, because we call the function.
(deftest side-effects-simple-case
  (let [env (create-new-environment)]
    (actual-value '(define (p1 x) (set! x (+ x 1)) x) env)
    (is (= 2 (actual-value '(p1 1) env)))))

;Although Cy's would return (1 2)
;since it forces all statements to be evaluated
;in this version, the argument e only gets one pass of an eval
;which resolves the variable to the thunk, but never evaluates the thunk
;and therefore the side-effect function is never evaluated.
(deftest side-effects-not-always-evaluated-with-lazy-evaluation
  (let [env (create-new-environment)]
    (actual-value
      '(define (p2 x)
         (define (p (e lazy-memo))
           e
           x)
       (p (set! x (+ x 1))))
      env)
    (is (= (actual-value '(p2 1) env) 1))
    ))

(deftest side-effects-always-evaluated-with-eager-evaluation
  (let [env (create-new-environment)]
    (actual-value
      '(define (p2 x)
         (define (p e)
           e
           x)
       (p (set! x (+ x 1))))
      env)
    (is (= (actual-value '(p2 1) env) 2))
    ))
