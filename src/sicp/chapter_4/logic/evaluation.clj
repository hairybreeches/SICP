(ns sicp.chapter-4.logic.evaluation)

(defmulti qeval (fn [query frames] (type query)))
