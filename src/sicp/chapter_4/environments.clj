(ns sicp.chapter-4.environments
  (:use sicp.error))

;variables
(defn- set-value
  [variable value]
  (dosync (ref-set (second variable) value)))

(defn- make-variable
  [var-name var-value]
  (list var-name (ref var-value)))

(defn- variable-name
  [var]
  (first var))

(defn- variable-value
  [var]
  @(second var))

(defn- named?
  [n variable]
  (= n (variable-name variable)))

;frames
(defn- get-frame-variables
  [frame]
  @frame)

(defn- make-frame
  [variables values]
  (ref (map make-variable variables values)))

(defn- append-to-frame
  [frame-value var value]
  (cons (make-variable var value) frame-value))

(defn- add-binding-to-frame!
  [var value frame]
  (dosync
    (alter frame (fn [fr] (append-to-frame fr var value)))))

(defn- get-variable-from-frame
  [var-name frame]
  (loop [variables (get-frame-variables frame)]
    (cond (empty? variables) nil
          (named? var-name (first variables)) (first variables)
          :else (recur (rest variables)))))

;environments
(defn- enclosing-environment
  [env]
  (rest env))

(defn- first-frame
  [env]
  (first env))

(def the-empty-environment
  '())

(defn- get-variable-from-environment
  [var-name env]
  (loop [env env]
  (if (= env the-empty-environment)
      (error "Unbound variable: " var-name)
      (let [first-frame-variable (get-variable-from-frame var-name (first-frame env))]
        (if (nil? first-frame-variable)
            (recur (enclosing-environment env))
            first-frame-variable)))))

;public functions
(defn lookup-variable-value
  [var-name env]
  (variable-value
    (get-variable-from-environment var-name env)))

(defn extend-environment
  [variables values base-env]
  (if (= (count variables) (count values))
      (cons (make-frame variables values) base-env)
      (if (> (count variables) (count values))
          (error "Too few arguments supplied, needed values for: " variables " got values: " values)
          (error "Too many arguments supplied, needed values for: " variables "got values: " values))))


(defn define-variable!
  [var-name value env]
  (let [frame (first-frame env)
         existing (get-variable-from-frame var-name frame)]
       (if (nil? existing)
           (add-binding-to-frame! var-name value frame)
           (set-value existing value))))

(defn set-variable-value!
  [var-name value env]
  (set-value
    (get-variable-from-environment var-name env)
    value))
