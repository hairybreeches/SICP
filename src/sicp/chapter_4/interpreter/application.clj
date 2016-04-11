(ns sicp.chapter-4.interpreter.application
  (:use sicp.chapter-4.interpreter.evaluator))

(defn- get-args
  [aprocs env succeed fail]
  (if (empty? aprocs)
      (succeed '() fail)
      ((first aprocs)
       env
       (fn [arg fail2]
         (get-args
           (rest aprocs)
           env
           (fn [args fail3]
             (succeed (cons arg args) fail3))
           fail2))
       fail)))

(defn- analyse-application [exp]
  (let [fproc (analyse (operator exp))
        aprocs (map analyse (operands exp))]
    (fn [env succeed fail]
      (fproc
        env
        (fn [proc fail2]
            (get-args
              aprocs
              env
              (fn [args fail3]
                (execute-application proc args succeed fail3))
              fail2))
        fail))))

(defmethod analyse :default [exp]
  (analyse-application exp))
