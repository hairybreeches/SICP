(ns sicp.chapter-4.environments)

(defn- enclosing-environment
  [env]
  (rest env))

(defn- first-frame
  [env]
  (first env))

(def the-empty-environment
  '())

(defn lookup-variable-value
  [var env])

(defn extend-environment
  [variables values base-env])

(defn define-variable!
  [var value env])

(defn set-variable-value!
  [var value env])
