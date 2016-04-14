(ns sicp.chapter-4.logic.simple-queries
  (:use sicp.chapter-4.logic.evaluation))

(defn- find-assertions [query-pattern frame]
  )

(defn- apply-rules [query-pattern frame]
  )

(defmethod qeval :default [query-pattern frames]
  (mapcat
    #(concat
       (find-assertions query-pattern %)
       (apply-rules query-pattern %))
    frames))
