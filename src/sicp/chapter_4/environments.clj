(ns sicp.chapter-4.environments
  (:use sicp.error))

(defn- make-value
  [value]
  (ref value))

(defn- retrieve-value
  [value]
  @value)

(defn- make-frame-value
  [variables values]
  (list variables values))

(defn- make-frame
  [variables values]
  (ref (make-frame-value variables (map make-value values))))

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
    (cons (make-value value) (frame-values frame))))

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
  [var env]
  (letfn [(env-loop [env]
                    (letfn [(scan [vars values]
                                  (cond (empty? vars) (env-loop (enclosing-environment env))
                                        (= var (first vars)) (retrieve-value (first values))
                                        :else (scan (rest vars) (rest values))))]

                    (if (= env the-empty-environment)
                        (error "Unbound variable" var)
                      (let [frame (first-frame env)]
                        (scan (frame-variables frame)
                              (frame-values frame))))))]
    (env-loop env)))

(defn extend-environment
  [variables values base-env]
  (if (= (count variables) (count values))
      (cons (make-frame variables values) base-env)
      (if (> (count variables) (count values))
          (error "Too many arguments supplied " variables values)
          (error "Too few arguments supplied " variables values))))


(defn define-variable!
  [var value env]
  )

(defn set-variable-value!
  [var value env]
  (letfn [(env-loop [env]
                    (letfn [(scan [vars values]
                                  (cond (empty? vars) (env-loop (enclosing-environment env))
                                        (= var (first vars)) (dosync (ref-set (first values) value))
                                        :else (scan (rest vars) (rest values))))]
                      (if (= env the-empty-environment)
                          (error "Unbound variable:" var)
                          (let [frame (first-frame env)]
                            (scan (frame-variables frame)
                                  (frame-values frame))))))]
    (env-loop env)))
