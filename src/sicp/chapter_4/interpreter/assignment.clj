(ns sicp.chapter-4.interpreter.assignment
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.environments))


(defn- assignment-variable
  [exp]
  (first (operands exp)))

(defn- assignment-value
  [exp]
  (second (operands exp)))

(defn- analyse-assignment
  [exp]
  (let [var-name (assignment-variable exp)
        value-proc (analyse (assignment-value exp))]
    (fn [env succeed fail]
      (value-proc
        env
        (fn [value fail2]
          (let [old-value (peek-variable var-name env)]
               (set-variable-value! var-name value env)
               (succeed
                 :ok
                 (fn []
                   (set-variable-value! var-name old-value env)
                   (fail2)))))
        fail))))

(defn- analyse-permanent-assignment
  [exp]
  (let [var-name (assignment-variable exp)
        value-proc (analyse (assignment-value exp))]
    (fn [env succeed fail]
      (value-proc
        env
        (fn [value fail2]
            (set-variable-value! var-name value env)
               (succeed
                 :ok
                 fail2))
        fail))))

(defmethod analyse 'set! [exp]
  (analyse-assignment exp))

(defmethod analyse 'permanent-set! [exp]
  (analyse-permanent-assignment exp))

(defn make-set
  [var-name var-value]
  (create-expression 'set! (list var-name var-value)))
