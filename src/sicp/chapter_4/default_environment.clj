(ns sicp.chapter-4.default-environment
  (:use sicp.chapter-4.environments)
  (:use sicp.chapter-4.procedures))

(defn create-new-environment
  []
  (let [initial-env (extend-environment primitive-procedure-names
                                        primitive-procedure-objects
                                        the-empty-environment)]
    initial-env))
