(ns sicp.chapter-4.let
  (:use sicp.chapter-4.evaluator)
  (:use sicp.chapter-4.begin)
  (:use sicp.chapter-4.lambda))

(defn- get-variable-declarations
  [exp]
  (second exp))

(defn- get-variable-names
  [exp]
  (map first (get-variable-declarations exp)))

(defn- get-variable-values
  [exp]
  (map second (get-variable-declarations exp)))

(defn- get-body
  [exp]
  (drop 2 exp))

(defn- let->combination
  [exp]
  (cons (make-lambda
          (get-variable-names exp)
          (get-body exp))
  (get-variable-values exp)))

(defmethod my-eval 'let [exp env]
  (my-eval (let->combination exp) env))

(defn make-let [variable-declarations body]
  (list 'let variable-declarations body))

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




