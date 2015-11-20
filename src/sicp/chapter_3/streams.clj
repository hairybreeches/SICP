(ns sicp.chapter-3.streams)

(defn memo-proc
  [proc]
  (let [run (ref false)
        value (ref false)]
    (fn []
      (if (not @run)
          (dosync
            (ref-set value (proc))
            (ref-set run true)))
      @value)))



(defn force-stream
  [value]
  (value))

(defmacro delay-stream
  [form]
  `(memo-proc (fn [] ~form)))

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

(defn empty-stream?
  [stream]
  (= (car-stream stream) :empty-stream))

(def empty-stream
  (cons-stream :empty-stream (throw (Exception. "Cannot find the cdr of the empty stream"))))

(defn stream->list
  [stream]
  (if (empty-stream? stream)
      `()
       (cons (car-stream stream)
             (stream->list (cdr-stream stream)))))

(defn stream-enumerate-interval
  [low high]
  (if (> low high)
      empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (inc low) high))))

(defn stream-map
  [proc & argstreams]
    (if (empty-stream? (first argstreams))
        empty-stream
        (cons-stream
           (apply proc (map car-stream argstreams))
           (apply stream-map proc (map cdr-stream argstreams)))))


