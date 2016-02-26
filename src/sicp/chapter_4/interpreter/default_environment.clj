(ns sicp.chapter-4.interpreter.default-environment
  (:use sicp.chapter-4.interpreter.environments)
  (:use sicp.chapter-4.interpreter.laziness)
  (:use sicp.chapter-4.interpreter.primitive-procedures))

(defn- import-lists
  [env]
  (actual-value
    '(begin
      (define (cons (x lazy-memo) (y lazy-memo))
         (lambda (m) (m x y)))

      (define (car z)
        (z (lambda (p q) p)))

      (define (cdr z)
        (z (lambda (p q) q))))

    env))




(defn create-new-environment
  []
  (let [initial-env
        (extend-environment '(null) '(null)
                    (extend-environment primitive-procedure-names
                                        primitive-procedure-objects
                                        the-empty-environment))]
    (import-lists initial-env)

    initial-env))
