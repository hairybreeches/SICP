(ns sicp.chapter-4.procedures)

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

(defn primitive-procedure?
  [procedure]
  (tagged-list? procedure 'primitive))

(defn- primitive-implementation
  [procedure]
  (second procedure))

(def primitive-procedures
  (list
    (list 'car first)
    (list 'cdr rest)
    (list 'cons cons)
    (list '= =)
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

(defn apply-primitive-procedure
  [procedure args]
  (apply-in-underlying-clojure (primitive-implementation procedure) args))
