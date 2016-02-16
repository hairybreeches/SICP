(ns sicp.chapter-4.interpreter.cond
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.if)
  (:use sicp.chapter-4.interpreter.let)
  (:use sicp.chapter-4.interpreter.begin)
  (:use sicp.chapter-4.interpreter.lambda)
  (:use sicp.error))

(defn- cond-clauses
  [exp]
  (rest exp))

(defn- cond-predicate
  [clause]
  (first clause))

(defn- cond-actions
  [clause]
  (rest clause))

(defn- cond-else-clause?
  [clause]
  (= (cond-predicate clause) 'else))

(defn- function-clause?
  [clause]
  (= (second clause) '=>))

(defn- clause-function
  [clause]
  (nth clause 2))

(defn- expand-clauses
  [clauses]
  (if
    (empty? clauses)
    false
    (let [first-clause (first clauses)
          rest-clauses (rest clauses)]
      (cond

        (cond-else-clause? first-clause)
        (if (empty? rest-clauses)
            (sequence->exp (cond-actions first-clause))
            (error "else clause not last: " clauses))

        (function-clause? first-clause)
        (let [result-name (gensym)]
          (make-let
            (list
              (list
                result-name
                (cond-predicate first-clause)))
            (make-if
              result-name
              (create-expression
                  (clause-function first-clause)
                  (list result-name))

              (expand-clauses rest-clauses))))

        :else
        (make-if (cond-predicate first-clause)
                 (sequence->exp (cond-actions first-clause))
                 (expand-clauses rest-clauses))))))

(defn cond->if
  [exp]
  (expand-clauses (cond-clauses exp)))

(defmethod my-eval 'cond [exp env]
  (my-eval (cond->if exp) env))
