(ns sicp.chapter-4.logic.query-syntax)

(defn query-content
  [exp]
  (rest exp))

(defn query-type
  [exp]
  (first exp))

(defn add-assertion-body [query]
  (first (query-content query)))

(defn assertion-to-be-added? [query]
  (= (query-type query) 'assert!))

(defn negated-query [operands]
  (first operands))

(defn predicate [exp]
  (first exp))

(defn args [exp]
  (rest exp))

(defn rule? [exp]
  (= (query-type exp) 'rule))

(defn conclusion
  [rule]
  (second rule))

(defn rule-body
  [rule]
  (if (< (count rule) 3)
     '(always-true)
    (nth rule 2)))

(defn contract-question-mark [variable frame]
  (symbol (str "?"
               (if (number? (second variable))
                 (str (nth variable 2)
                      "-"
                      (second variable))
                 (second variable)))))

(defn expand-question-mark [sym]
  (let [s (name sym)]
    (if (= "?" (first s))
      (list '? (sym (rest s)))
      sym)))

(defn- map-over-symbols [proc exp]
  (cond (seq? exp) (cons
                     (map-over-symbols proc (first exp))
                     (map-over-symbols proc (rest exp)))
        (symbol? exp) (proc exp)
        :else exp))

(defn query-syntax-process [input]
  (map-over-symbols expand-question-mark input))

(defn variable? [exp]
  (= (query-type exp) '?))

(defn constant-symbol? [exp]
  (symbol? exp))

(defn make-new-variable
  [variable id]
  (cons '? (cons id (rest variable))))
