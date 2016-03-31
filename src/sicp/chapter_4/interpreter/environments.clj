(ns sicp.chapter-4.interpreter.environments
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
(defmethod print-method ::frame
  [v w]
  (.write
    w
    (str "<Frame: " (apply list (map variable-name v)) ">")))

(defn- get-frame-variables
  [frame]
  @frame)

(defn- make-frame-value
  [variables]
  (with-meta
        variables
        {:type ::frame}))

(defn- make-frame
  ([variables values]
    (make-frame
      (map make-variable variables values)))

  ([variables]
    (ref
      (make-frame-value variables))))

(defn- append-to-frame
  [frame-value var value]
  (make-frame-value
    (cons
      (make-variable var value)
      frame-value)))

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

(defn filter-value
  [variables var-name]
    (make-frame-value
      (remove #(named? var-name %) variables)))


(defn- remove-frame-binding!
  [var-name frame]
  (if (nil? (get-variable-from-frame var-name frame))
      false
      (dosync
        (alter frame filter-value var-name)
        true)))

;environments
(defn- make-environment
  [frames]
  (with-meta
        frames
        {:type ::environment}))

(defmethod print-method ::environment
  [v w]
  (.write
    w
    (str (apply list (map (fn [frame] @frame) v)))))

(defn enclosing-environment
  [env]
  (make-environment
    (rest env)))

(defn- first-frame
  [env]
  (first env))

(def the-empty-environment
  (make-environment
  '()))

(defn- get-variable-from-environment
  [var-name env]
  (loop [env env]
  (if (= env the-empty-environment)
      (error "Unbound variable: " var-name)
      (let [first-frame-variable (get-variable-from-frame var-name (first-frame env))]
        (if (nil? first-frame-variable)
            (recur (enclosing-environment env))
            first-frame-variable)))))

(defn- remove-binding!
  [env var-name]
  (loop [env env]
    (cond (= env the-empty-environment) (error "variable not bound! " var-name)
          (remove-frame-binding! var-name (first-frame env)) true
          :else (recur (enclosing-environment env)))))

;public functions
(defn lookup-variable-value
  [var-name env]
  (let [value (variable-value
    (get-variable-from-environment var-name env))]
    (if (= value '*unassigned*)
        (error "variable " var-name " is unassigned")
        value)))

(defn extend-environment
  [variables values base-env]
  (let [num-variables (count variables)
        num-values (count values)
        list-variables (apply list variables)
        list-values (apply list values)]
  (cond (= num-values num-variables) (make-environment (cons (make-frame list-variables list-values) base-env))
        (> num-variables num-values) (error "Too few arguments supplied, needed values for: " list-variables " got values: " list-values)
        :else (error "Too many arguments supplied, needed values for: " list-variables "got values: " list-values))))


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

;unbinding works like set - it operates on the first definition it can find.
;this makes sense - you don't want to affect global scope you don't understand!
(defn make-unbound!
  [var-name env]
  (remove-binding! env var-name))
