(ns sicp.chapter-3.streams)

(defn force-stream
  [value]
  (value))

(defmacro delay-stream
  [form]
  `(fn [] ~form))

(defmacro cons-stream
  [car-form cdr-form]
  `(let [cdr# (delay-stream ~cdr-form)
         car# ~car-form]
     (fn [m#]
         (cond (= m# :car)
               car#

               (= m# :cdr)
               cdr#))))

(defn car-stream
  [stream]
  (stream :car))

(defn cdr-stream
  [stream]
  (force-stream (stream :cdr)))
