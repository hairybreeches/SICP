(ns sicp.chapter-4.logic.evaluation)

(defn- expression-type [query]
  )

(defmulti qeval (fn [query frames] (expression-type query)))
