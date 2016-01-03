(ns sicp.chapter-4.cond
  (:use sicp.chapter-4.evaluator)
  (:use sicp.chapter-4.if)
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

(defn- expand-clauses
  [clauses]
  (if
    (empty? clauses)
    false
    (let [first-clause (first clauses)
          rest-clauses (rest clauses)]
      (if (cond-else-clause? first-clause)
          (if (empty? rest-clauses)
              (sequence->exp (cond-actions first-clause))
              (error "else clause not last: " clauses))
          (make-if (cond-predicate first-clause)
                   (sequence->exp (cond-actions first-clause))
                   (expand-clauses rest-clauses))))))

(defn- cond->if
  [exp]
  (expand-clauses (cond-clauses exp)))

(defmethod eval-list-expression 'cond [exp env]
  (my-eval (cond->if exp) env))
