(ns sicp.chapter-4.interpreter.default-environment
  (:use sicp.chapter-4.interpreter.environments)
  (:use sicp.chapter-4.interpreter.primitive-procedures))

(defn create-new-environment
  []
  (let [initial-env
        (extend-environment '(null) '(null)
                    (extend-environment primitive-procedure-names
                                        primitive-procedure-objects
                                        the-empty-environment))]
    initial-env))
