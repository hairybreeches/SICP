(ns sicp.test.chapter-4.environments
  (:use sicp.chapter-4.environments)
  (:use clojure.test))

(deftest can-retrieve-value-in-extended-environment
  (is (= 4
         (lookup-variable-value
           'x
           (extend-environment '(x) '(4) the-empty-environment)))))

(deftest can-set-existing-value
  (let [env (extend-environment '(x) '(12) the-empty-environment)]
    (set-variable-value! 'x 5 env)
    (is (= (lookup-variable-value 'x env) 5))))

(deftest can-define-existing-value
  (let [env (extend-environment '(x) '(12) the-empty-environment)]
    (define-variable! 'x 5 env)
    (is (= (lookup-variable-value 'x env) 5))))

(deftest can-define-non-existing-value
  (let [env (extend-environment '() '() the-empty-environment)]
    (define-variable! 'x 5 env)
    (is (= (lookup-variable-value 'x env) 5))))

(deftest definition-in-inner-scope
  (let [outer (extend-environment '(y) '("outer value") the-empty-environment)
        inner (extend-environment '() '() outer)]
    (define-variable! 'y "inner value" inner)
    (is (= (lookup-variable-value 'y outer) "outer value"))
    (is (= (lookup-variable-value 'y inner) "inner value"))))

(deftest extension-in-inner-scope
  (let [outer (extend-environment '(y) '("outer value") the-empty-environment)
        inner (extend-environment '(y) '("inner value") outer)]
    (is (= (lookup-variable-value 'y outer) "outer value"))
    (is (= (lookup-variable-value 'y inner) "inner value"))))



