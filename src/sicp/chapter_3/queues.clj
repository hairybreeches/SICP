(ns sicp.chapter-3.queues
  (:use sicp.chapter-3.lists)
  (:use sicp.chapter-2.pairs))

(defn- front-pointer
  [queue]
  ((queue :get-front)))

(defn- rear-pointer
  [queue]
  ((queue :get-rear)))

(defn- set-rear-pointer!
  [queue item]
  ((queue :set-rear) item))

(defn- set-front-pointer!
  [queue item]
  ((queue :set-front) item))

(defn make-queue
  []
  (let [front-pointer (ref nil)
        rear-pointer (ref nil)]

    (defn front-pointer-private
      []
      @front-pointer)

    (defn rear-pointer-private
      []
      @rear-pointer)

    (defn set-front-pointer-private!
      [item]
      (dosync
       (ref-set front-pointer item)))

    (defn set-rear-pointer-private!
      [item]
      (dosync
       (ref-set rear-pointer item)))

    (defn dispatch
      [m]
      (cond (= m :set-front) set-front-pointer-private!
            (= m :set-rear) set-rear-pointer-private!
            (= m :get-front) front-pointer-private
            (= m :get-rear) rear-pointer-private))
    dispatch))

(defn empty-queue?
  [queue]
  (nil? (front-pointer queue)))

(defn front-queue
  [queue]
  (if (empty-queue? queue)
      nil
      (car (front-pointer queue))))

(defn insert-queue!
  [queue item]
  (let [new-pair (cons-pair item nil)]
    (if (empty-queue? queue)
          (do
            (set-front-pointer! queue new-pair)
            (set-rear-pointer! queue new-pair)
            queue)
          (do
            (set-cdr! (rear-pointer queue) new-pair)
            (set-rear-pointer! queue new-pair)
            queue))))

(defn delete-queue!
  [queue]
  (if (empty-queue? queue)
      queue
      (do
        (set-front-pointer! queue (cdr (front-pointer queue)))
        queue)))

