(ns sicp.test.chapter-4.environments
  (:use sicp.chapter-4.interpreter.environments)
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

(deftest to-string-after-extend
  (let [env (extend-environment '(x) '(12) the-empty-environment)]
    (is (= (str env) "(<Frame: (x)>)"))))

(deftest to-string-after-set
  (let [env (extend-environment '(x) '(12) the-empty-environment)]
    (set-variable-value! 'x 4 env)
    (is (= (str env) "(<Frame: (x)>)"))))

(deftest to-string-after-define
  (let [env (extend-environment '() '() the-empty-environment)]
    (define-variable! 'x 12 env)
    (is (= (str env) "(<Frame: (x)>)"))))

(deftest to-string-after-undefine
  (let [env (extend-environment '(x y) '(11 12) the-empty-environment)]
    (make-unbound! 'x env)
    (is (= (str env) "(<Frame: (y)>)"))))

(deftest to-string-after-double-extend
  (let [env (extend-environment '(z a) '(4 5) (extend-environment '(x y) '(11 12) the-empty-environment))]
    (is (= (str env) "(<Frame: (z a)> <Frame: (x y)>)"))))

(deftest to-string-after-taking-enclosing
  (let [env (extend-environment '(z a) '(4 5) (extend-environment '(x y) '(11 12) the-empty-environment))]
    (is (= (str (enclosing-environment env) "(<Frame: (x y)>)")))))



