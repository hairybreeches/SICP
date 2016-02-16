(ns sicp.chapter-4.interpreter.cond
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.primitive-datatypes)
  (:use sicp.chapter-4.interpreter.begin)
  (:use sicp.error))

;syntax
(defn- cond-clauses
  [exp]
  (operands exp))

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

;analysed clauses
(defn- analysed-clause-action
  [clause]
  (second clause))

(defn- analysed-clause-predicate
  [clause]
  (first clause))

(defn- make-analysed-clause
  [predicate action]
  (list predicate action))

;analysis of clauses
(defn- analyse-predicate
  [clause]
  (if (cond-else-clause? clause)
      'else
      (analyse (cond-predicate clause))))

(defn- analyse-statement-action
  [clause]
  (let [action (analyse (sequence->exp (cond-actions clause)))]
    (fn [result env]
      (action env))))

(defn- analyse-lambda-action
  [clause]
  (let [lambda (analyse (clause-function clause))]
    (fn [result env]
      (execute-application lambda (list result)))))

(defn- analyse-action
  [clause]
  (if (function-clause? clause)
      (analyse-lambda-action clause)
      (analyse-statement-action clause)))

(defn- analyse-clause
  [clause]
  (make-analysed-clause (analyse-predicate clause) (analyse-action clause)))

(defn- analyse-clauses
  [clauses]
  (if (empty? clauses)
      '()
    (let [first-clause (first clauses)
          rest-clauses (rest clauses)]
      (if (cond-else-clause? first-clause)
          (if (empty? rest-clauses)
              (list (make-analysed-clause (fn [env] true) (analyse-action first-clause)))
              (error "else clause not last: " clauses))
          (cons (analyse-clause first-clause) (analyse-clauses rest-clauses))))))

; analysis of cond
(defn- analyse-cond
  [analysed-clauses]
  (fn [env]
    (loop [clauses analysed-clauses]
      (if
        (empty? clauses)
        false
        (let [predicate-result ((analysed-clause-predicate (first clauses)) env)]
            (if (my-true? predicate-result)
                ((analysed-clause-action (first clauses)) predicate-result env)
                (recur (rest clauses))))))))

(defmethod analyse 'cond [exp]
  (analyse-cond (analyse-clauses (cond-clauses exp))))
