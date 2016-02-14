(ns sicp.chapter-4.interpreter.if
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.primitive-datatypes))

(defn- if-predicate
  [exp]
  (first (operands exp)))

(defn- if-consequent
  [exp]
  (second (operands exp)))

(defn- if-alternative
  [exp]
  (if (empty? (drop 2 (operands exp)))
      false
      (nth (operands exp) 2)))

(defn- analyse-if
  [exp]
  (let [predicate (analyse (if-predicate exp))
        consequent (analyse (if-consequent exp))
        alternative (analyse (if-alternative exp))]
    (fn [env]
      (if (my-true? (predicate env))
          (consequent env)
          (alternative env)))))

(defn make-if
  [predicate consequent alternative]
  (create-expression 'if (list predicate consequent alternative)))

(defmethod analyse 'if [exp]
  (analyse-if exp))
