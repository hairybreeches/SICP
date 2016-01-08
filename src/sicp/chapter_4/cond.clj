(ns sicp.chapter-4.cond
  (:use sicp.chapter-4.evaluator)
  (:use sicp.chapter-4.primitive-datatypes)
  (:use sicp.chapter-4.begin)
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

(defn- eval-cond
  [clauses env]
  (loop [clauses clauses]
  (if
    (empty? clauses)
    false
    (let [first-clause (first clauses)
          rest-clauses (rest clauses)]
      (if (cond-else-clause? first-clause)
          (if (empty? rest-clauses)
              (sequence->exp (cond-actions first-clause))
              (error "else clause not last: " clauses))
          (let [predicate-result (my-eval (cond-predicate first-clause) env)]
               (if (my-true? predicate-result)
                   (if (function-clause? first-clause)
                       (my-apply (my-eval (clause-function first-clause) env) (list predicate-result))
                       (my-eval (sequence->exp (cond-actions first-clause)) env))
                   (recur rest-clauses))))))))

(defmethod my-eval 'cond [exp env]
  (eval-cond (rest exp) env))
