(ns sicp.chapter-4.interpreter.unless
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.if)
  (:use sicp.chapter-4.interpreter.boolean-operators))

(defn- unless-predicate
  [exp]
  (first (operands exp)))

(defn- unless-consequent
  [exp]
  (second (operands exp)))

(defn- unless-alternative
  [exp]
  (nth (operands exp) 2))

(defn- has-alternative?
  [exp]
  (> (count (operands exp)) 2))

(defn- unless->if
  [exp]
  (let [if-predicate (make-not (unless-predicate exp))
        consequent (unless-consequent exp)]
    (if (has-alternative? exp)
      (make-if
        if-predicate
        consequent
        (unless-alternative exp))
      (make-if
        if-predicate
        consequent))))

(defmethod analyse 'unless [exp]
  (analyse (unless->if exp)))


