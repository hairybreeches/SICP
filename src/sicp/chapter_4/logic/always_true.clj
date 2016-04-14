(ns sicp.chapter-4.logic.always-true
  (:use sicp.chapter-4.logic.evaluation))

(defn- always-true [ignore frames]
  frames)

(defmethod qeval :always-true [query-pattern frames]
  (always-true query-pattern frames))
