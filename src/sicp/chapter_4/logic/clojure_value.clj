(ns sicp.chapter-4.logic.clojure-value
  (:use sicp.error)
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.evaluation))

(defn name-before
  [n1 n2]
  (> (compare (name (first n1)) (name (first n2))) 0))

(defn- execute [exp]
  (apply (eval (predicate exp))
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

(defmethod qeval-dispatch 'clojure-value [_ query-pattern frames]
  (clojure-value query-pattern frames))
