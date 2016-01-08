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
