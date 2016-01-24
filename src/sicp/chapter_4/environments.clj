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

;environments
(defn- enclosing-environment
  [env]
  (rest env))

(defn- first-frame
  [env]
  (first env))

(def the-empty-environment
  '())

;public functions
(defn lookup-variable-value
  [var env]
  (letfn [(env-loop [env]
                    (letfn [(scan [vars]
                                  (cond (empty? vars) (env-loop (enclosing-environment env))
                                        (= var (variable-name (first vars))) (variable-value (first vars))
                                        :else (scan (rest vars))))]

                    (if (= env the-empty-environment)
                        (error "Unbound variable: " var)
                      (let [frame (first-frame env)]
                        (scan (get-frame-variables frame))))))]
    (env-loop env)))

(defn extend-environment
  [variables values base-env]
  (if (= (count variables) (count values))
      (cons (make-frame variables values) base-env)
      (if (> (count variables) (count values))
          (error "Too few arguments supplied, needed values for: " variables " got values: " values)
          (error "Too many arguments supplied, needed values for: " variables "got values: " values))))


(defn define-variable!
  [var value env]
  (let [frame (first-frame env)]
    (loop [vars (get-frame-variables frame)]
      (cond (empty? vars) (add-binding-to-frame! var value frame)
            (= var (variable-name (first vars))) (set-value (first vars) value)
            :else (recur (rest vars))))))

(defn set-variable-value!
  [var value env]
  (letfn [(env-loop [env]
                    (letfn [(scan [vars]
                                  (cond (empty? vars) (env-loop (enclosing-environment env))
                                        (= var (variable-name (first vars))) (set-value (first vars) value)
                                        :else (scan (rest vars))))]
                      (if (= env the-empty-environment)
                          (error "Unbound variable:" var)
                          (let [frame (first-frame env)]
                            (scan (get-frame-variables frame))))))]
    (env-loop env)))
