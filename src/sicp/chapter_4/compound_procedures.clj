(ns sicp.chapter-4.compound-procedures)

(defn tagged-list?
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
