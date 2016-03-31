(ns sicp.chapter-4.interpreter.begin
  (:use sicp.chapter-4.interpreter.evaluator))

(defn- begin-actions
  [exp]
  (operands exp))

(defn- make-begin
  [actions]
  (create-expression 'begin actions))

(defn- last?
  [exps]
  (empty? (rest exps)))

(defn- sequentially
  [proc1 proc2]
  (fn [env succeed fail]
    (proc1
      env
      (fn [proc1-value fail2]
          (proc2 env succeed fail2))
      fail)))

(defn- analyse-sequence
  [exps]
  (let [procs (map analyse exps)]
  (loop [first-proc (first procs)
         rest-procs (rest procs)]
    (if (empty? rest-procs)
        first-proc
        (recur (sequentially first-proc (first rest-procs))
               (rest rest-procs))))))

(defn sequence->exp
  [actions]
  (cond (empty? actions) actions
        (last? actions) (first actions)
        :else (make-begin actions)))

(defmethod analyse 'begin [exp]
  (analyse-sequence
    (begin-actions exp)))
