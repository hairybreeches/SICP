(ns sicp.chapter-4.environments)

(defn- make-frame-value
  [variables values]
  (list variables values))

(defn- make-frame
  [variables values]
  (ref (make-frame-value variables values)))


(defn- frame-variables
  [frame]
  (first @frame))

(defn- frame-values
  [frame]
  (second @frame))

(defn- append-to-frame
  [frame var value]
  (make-frame-value
    (cons var (frame-variables frame))
    (cons value (frame-values frame))))

(defn- add-binding-to-frame!
  [var value frame]
  (alter frame (fn [fr] (append-to-frame fr var value))))


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
