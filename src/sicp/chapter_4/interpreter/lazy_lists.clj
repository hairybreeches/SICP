(ns sicp.chapter-4.interpreter.lazy-lists
  (:use sicp.chapter-4.interpreter.evaluator)
  (:use sicp.chapter-4.interpreter.laziness)
  (:use sicp.error))


(defn- verify-lazy-pair
  [x]
  (if (not (= (type x) ::lazy-pair))
      (error "Need a lazy pair, got something of type " (type x) " : " x)))

(defn- eval-lazy-pair
  [a b env]
  ^{:type ::lazy-pair}
  {
    :car (delay-it a env)
    :cdr (delay-it b env)
  })

(defn car
  [x]
  (force-it (:car x)))

(defn cdr
  [x]
  (force-it (:cdr x)))

(defn lazy-list->seq
  [x]
  (if (= x 'null)
      '()
      (do
        (verify-lazy-pair x)
        (lazy-seq
          (cons
            (car x)
            (lazy-list->seq (cdr x)))))))

(defn lazy-list->list
  [x]
  (apply list (lazy-list->seq x)))

(defn- printable-subsequence
  [s]
  (let [tes (take 21 (lazy-list->seq s))]
    (if (= (count tes) 21)
        (concat (take 20 tes) (list '...))
        (take 20 tes))))


(defmethod print-method ::lazy-pair
  [v w]
  (.write
    w
    (str (apply list (printable-subsequence v)))))

(defmethod my-eval 'cons [exp env]
  (eval-lazy-pair (first (operands exp))
                  (second (operands exp))
                  env))

(defmethod my-eval 'car [exp env]
  (let [l (my-eval (first (operands exp)) env)]
    (verify-lazy-pair l)
    (car l)))

(defmethod my-eval 'cdr [exp env]
  (let [l (my-eval (first (operands exp)) env)]
    (verify-lazy-pair l)
    (cdr l)))
