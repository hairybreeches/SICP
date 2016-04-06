(ns sicp.chapter-4.interpreter.primitive-procedures
  (:use clojure.math.numeric-tower)
  (:use sicp.chapter-4.interpreter.evaluator))

(defn- primitive-implementation
  [procedure]
  (second procedure))

(def primitive-procedures
  (list
    (list 'abs abs)
    (list 'distinct? distinct?)
    (list 'int int)
    (list 'list list)
    (list 'integer? integer?)
    (list 'sqrt sqrt)
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

(defmethod my-apply 'primitive
  [procedure args]
  (apply-in-underlying-clojure (primitive-implementation procedure) args))
