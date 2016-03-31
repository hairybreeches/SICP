(ns sicp.chapter-4.interpreter.while
  (:use sicp.chapter-4.interpreter.if)
  (:use sicp.chapter-4.interpreter.begin)
  (:use sicp.chapter-4.interpreter.let)
  (:use sicp.chapter-4.interpreter.evaluator))

(defn- get-body
  [exp]
  (second (operands exp)))

(defn- get-predicate
  [exp]
  (first (operands exp)))

(defn- make-while
  [predicate body]
  (create-expression 'while (list predicate body)))

(defmethod my-eval 'while [exp env]
  (my-eval
    (make-if (get-predicate exp)
             (sequence->exp
               (list
                 (get-body exp)
                 exp))
             true)
    env))
(defmethod analyse 'while [exp]
  (let [action (analyse (get-body exp))
        predicate (analyse (get-predicate exp))]
      (fn [env]
        (loop []
          (if (predicate env)
              (do (action env)
                  (recur))
              true)))))
(defn- while->if
  [exp]
  (let [func-name (gensym)]
    (make-named-let
      func-name
      (list)
      (make-if
        (get-predicate exp)
        (sequence->exp
           (list
              (get-body exp)
              (create-expression func-name '())))
        true))))

(defmethod analyse 'while [exp env]
     (analyse
       (while->if exp)
       env))


