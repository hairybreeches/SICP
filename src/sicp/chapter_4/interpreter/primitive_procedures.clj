(ns sicp.chapter-4.interpreter.primitive-procedures
  (:use sicp.chapter-4.interpreter.evaluator))

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
    (list '/ /)
    (list 'prn prn)))

(def primitive-procedure-names
  (map first primitive-procedures))

(def primitive-procedure-objects
  (map #(list 'primitive (second %)) primitive-procedures))

(defn- apply-in-underlying-clojure
  [procedure args]
  (apply procedure args))

(defmethod execute-application 'primitive
  [procedure args]
  (apply-in-underlying-clojure (primitive-implementation procedure) args))
