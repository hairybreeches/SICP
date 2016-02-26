(ns sicp.chapter-4.interpreter.quote
  (:use sicp.chapter-4.interpreter.evaluator))

(defn text-of-quotation
  [exp]
  (first (operands exp)))

(defn list->lazy-list
  [l]
  (if (empty? l)
      'null
      (create-expression
        'cons
        (list
          (create-expression 'quote (list (first l)))
          (list->lazy-list (rest l))))))

(defn to-lazy-lists
  [exp env]
  (cond (not (seq? exp)) exp
        :else (-> (map #(to-lazy-lists % env) exp)
                  (list->lazy-list)
                  (my-eval env))))

(defmethod my-eval 'quote [exp env]
  (let [result (to-lazy-lists (text-of-quotation exp) env)]
  result))
