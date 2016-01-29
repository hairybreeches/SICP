(ns sicp.chapter-4.procedures
  (:use sicp.chapter-4.evaluator)
  (:use sicp.chapter-4.environments))

(defn- tagged-list?
  [exp tag]
  (if (seq? exp)
      (= (first exp) tag)
      false))

(defn compound-procedure?
  [p]
  (tagged-list? p 'procedure))

(defn make-procedure
  [parameters body env]
  (list 'procedure parameters body env))

(defn procedure-body
  [p]
  (nth p 2))

(defn procedure-parameters
  [p]
  (second p))

(defn procedure-environment
  [p]
  (nth p 3))

(defn- primitive-implementation
  [procedure]
  (second procedure))

(def primitive-procedures
  (list
    (list 'car first)
    (list 'cdr rest)
    (list 'cons cons)
    (list 'null? empty?)
    (list '= =)
    (list '> >)
    (list '< <)
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)))

(def primitive-procedure-names
  (map first primitive-procedures))

(def primitive-procedure-objects
  (map #(list 'primitive (second %)) primitive-procedures))

(defn- apply-in-underlying-clojure
  [procedure args]
  (apply procedure args))

(defmethod my-apply 'procedure
  [procedure arguments]
  (eval-sequence
    (procedure-body procedure)
    (extend-environment (procedure-parameters procedure)
                        arguments
                        (procedure-environment procedure))))

(defmethod my-apply 'primitive
  [procedure args]
  (apply-in-underlying-clojure (primitive-implementation procedure) args))
