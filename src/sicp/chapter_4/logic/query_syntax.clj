(ns sicp.chapter-4.logic.query-syntax
  (:use sicp.error)
  (:use sicp.sequences))

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

(defn contract-question-mark [variable]
  (symbol (str "?"
               (if (= 3 (count variable))
                 (str (nth variable 2)
                      "-"
                      (second variable))
                 (second variable)))))

(defn expand-question-mark [sym]
  (let [s (name sym)]
    (if (= \? (first s))
      (list '? (symbol (apply str (rest s))))
      sym)))

(defn- map-over-symbols [proc exp]
  (cond (non-empty-seq? exp) (cons
                     (map-over-symbols proc (first exp))
                     (map-over-symbols proc (rest exp)))
        (symbol? exp) (proc exp)
        :else exp))

(defn query-syntax-process [input]
  (map-over-symbols expand-question-mark input))

(defn variable? [exp]
  (and
    (seq? exp)
    (= (first exp) '?)))


(defn constant-symbol? [exp]
  (symbol? exp))

(defn make-new-variable
  [variable id]
  (cons '? (cons id (rest variable))))


(defn remove-dots
  [exp]
  (cond (not (non-empty-seq? exp)) exp
        (not (= '. (first exp))) (cons (remove-dots (first exp)) (remove-dots (rest exp)))
        (not (= 2 (count exp))) (error "cannot use dotted notation with more than one subsequent: " exp)
        (seq? (second exp)) (remove-dots (second exp))
        :else exp))


