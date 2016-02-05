(ns sicp.chapter-4.compound-procedures
  (:use sicp.chapter-4.evaluator)
  (:use sicp.chapter-4.begin)
  (:use sicp.chapter-4.environments)
  (:use sicp.chapter-4.lambda)
  (:use sicp.chapter-4.definition)
  (:use sicp.chapter-4.assignment)
  (:use sicp.chapter-4.let))

(defn- tagged-list?
  [exp tag]
  (if (seq? exp)
      (= (first exp) tag)
      false))

(defn compound-procedure?
  [p]
  (tagged-list? p 'procedure))

(defn- hoist-variables
  [variable-names parsed-statements]
  (if (empty? variable-names)
      (sequence->exp parsed-statements)
     (make-let
        (map list variable-names (repeat '(quote *unassigned*)))
        (sequence->exp parsed-statements))))

(defn- parse-defines
  [action-list]
  (loop [variable-names '()
         action-list action-list
         parsed-statements '()]
    (if (empty? action-list) {:variable-names variable-names :statements (reverse parsed-statements)}
        (let [current (first action-list)]
              (if (define? current)
                  (recur
                    (cons (definition-variable current) variable-names)
                    (rest action-list)
                    (cons (make-set (definition-variable current) (definition-value current))
                          parsed-statements))
                (recur variable-names
                       (rest action-list)
                       (cons current parsed-statements)))))))

(defn- scan-out-defines
  [action-list]
  (let [result (parse-defines action-list)]
    (hoist-variables (:variable-names result) (:statements result))))

(defn- make-procedure
  [parameters action-list env]
  (list 'procedure parameters (scan-out-defines action-list) env))

(defn procedure-body
  [p]
  (nth p 2))

(defn procedure-parameters
  [p]
  (second p))

(defn- procedure-environment
  [p]
  (nth p 3))

(defmethod my-apply 'procedure
  [procedure arguments]
  (my-eval
    (procedure-body procedure)
    (extend-environment (procedure-parameters procedure)
                        arguments
                        (procedure-environment procedure))))

(defmethod my-eval 'lambda [exp env]
  (make-procedure
    (lambda-parameters exp)
    (lambda-body exp)
    env))
