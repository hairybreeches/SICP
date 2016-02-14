(ns sicp.chapter-4.interpreter.let
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.begin)
  (:use sicp.error)
  (:use sicp.chapter-4.interpreter.definition)
  (:use sicp.chapter-4.interpreter.assignment)
  (:use sicp.chapter-4.interpreter.lambda))

(defn- named-let?
  [exp]
  (cond (symbol? (first (operands exp))) true
        (seq? (first (operands exp))) false
        :else (error "unknown let type: " exp)))

(defn- get-variable-declarations
  [exp]
  (if (named-let? exp)
      (second (operands exp))
      (first (operands exp))))

(defn- get-variable-names
  [exp]
  (map first (get-variable-declarations exp)))

(defn- get-variable-values
  [exp]
  (map second (get-variable-declarations exp)))

;unnamed lets
(defn- get-body
  [exp]
  (rest (operands exp)))

(defn- unnamed-let->combination
  [exp]
  (create-expression
    (make-lambda
          (get-variable-names exp)
          (get-body exp))
  (get-variable-values exp)))

;named lets
(defn- get-named-let-body
  [exp]
  (drop 2 (operands exp)))

(defn- get-named-let-name
  [exp]
  (if (not (named-let? exp))
      (error "not a named let!")
      (first (operands exp))))

(defn- get-named-let-definition
  [exp]
  (create-expression 'define (list (cons (get-named-let-name exp) (get-variable-names exp)) (sequence->exp (get-named-let-body exp)))))

(defn- named-let->combination
  [exp]
  (create-expression
    (make-lambda
      '()
      (list
        (get-named-let-definition exp)
        (create-expression (get-named-let-name exp) (get-variable-values exp))))
    '()
    ))

;general
(defn- let->combination
  [exp]
  (if (named-let? exp)
      (named-let->combination exp)
      (unnamed-let->combination exp)))

(defmethod my-eval 'let [exp env]
  (my-eval (let->combination exp) env))

(defn make-let [variable-declarations body]
  (create-expression 'let (list variable-declarations body)))

(defn- let*->nested-lets
  ([exp] (let*->nested-lets (get-variable-declarations exp) (get-body exp)))
  ([variable-declarations body]
   (if (empty? variable-declarations)
       (sequence->exp body)
       (make-let
         (list (first variable-declarations))
         (let*->nested-lets
           (rest variable-declarations)
           body)))))

(defmethod my-eval 'let* [exp env]
  (my-eval (let*->nested-lets exp) env))

(defn- make-unassigned
  [n]
  (list n '(quote *unassigned*)))

(defn- letrec->let
  [exp]
  (make-let
    (map make-unassigned (get-variable-names exp))
    (sequence->exp
      (concat
        (map make-set (get-variable-names exp) (get-variable-values exp))
        (get-body exp)))))

(defmethod my-eval 'letrec [exp env]
  (my-eval (letrec->let exp) env))




