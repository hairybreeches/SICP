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
    (fn [env succeed fail]
      (predicate
        env
        (fn [predicate-value fail2]
          (if (my-true? predicate-value)
              (consequent env succeed fail2)
              (alternative env succeed fail2)))
        fail))))

(defn make-if
  ([predicate consequent alternative]
    (create-expression 'if (list predicate consequent alternative)))
  ([predicate consequent]
    (create-expression 'if (list predicate consequent))))

(defmethod analyse 'if [exp]
  (analyse-if exp))
