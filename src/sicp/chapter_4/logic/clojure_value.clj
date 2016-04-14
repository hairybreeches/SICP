(ns sicp.chapter-4.logic.clojure-value
  (:use sicp.error)
  (:use sicp.chapter-4.logic.evaluation))

(defn- predicate [exp]
  (first exp))

(defn- args [exp]
  (rest exp))

(defn- execute [exp]
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(defn- clojure-value [call frames]
  (mapcat
    (fn [frame]
      (if (execute
            (instantiate
              call
              frame
              (fn [v f]
                (error "unknown pat var: " v))))
        (list frame)
        '()))
    frames))

(defmethod qeval :clojure-value [query-pattern frames]
  (clojure-value query-pattern frames))
