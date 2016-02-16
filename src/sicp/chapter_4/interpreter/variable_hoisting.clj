(ns sicp.chapter-4.interpreter.variable-hoisting
  (:use sicp.chapter-4.interpreter.begin)
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.definition)
  (:use sicp.chapter-4.interpreter.assignment)
  (:use sicp.chapter-4.interpreter.let))

(defn- scan-out-defines
  [variable-names parsed-statement]
  (sequence->exp
     (concat
        (map make-define variable-names (repeat '(quote *unassigned*)))
        (list parsed-statement))))

(defn- identity-hoist
  [exp]
  {:variable-names '() :statement exp})

(defmulti parse-defines get-exp-type)

(defmethod parse-defines 'lambda [exp] (identity-hoist exp))
(defmethod parse-defines java.lang.Boolean [exp] (identity-hoist exp))
(defmethod parse-defines java.lang.Number [exp] (identity-hoist exp))
(defmethod parse-defines java.lang.String [exp] (identity-hoist exp))
(defmethod parse-defines clojure.lang.Symbol [exp] (identity-hoist exp))
(defmethod parse-defines 'define [exp]
  {
    :variable-names (list (definition-variable exp))
    :statement (make-set (definition-variable exp) (definition-value exp))
  })

(defn- combine
  [summary-so-far current]
  {
    :variable-names (concat (:variable-names summary-so-far)
                           (:variable-names current))
    :statement (conj (:statement summary-so-far) (:statement current))
  })

(defmethod parse-defines :default
  [exp]
  (let [result (reduce
                  combine
                  {:variable-names '() :statement []}
                  (map parse-defines exp))]
    {:variable-names (:variable-names result)
     :statement (apply list (:statement result))}))

(defn hoist-variables
  [action-list]
  (let [result (parse-defines (sequence->exp action-list))]
    (scan-out-defines (:variable-names result) (:statement result))))
