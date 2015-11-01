(ns sicp.chapter-3.queues
  (:use sicp.chapter-3.lists)
  (:use sicp.chapter-2.pairs))

(defn- front-pointer
  [queue]
  (car queue))

(defn- rear-pointer
  [queue]
  (cdr queue))

(defn- set-rear-pointer!
  [queue item]
  (set-cdr! queue item))

(defn- set-front-pointer!
  [queue item]
  (set-car! queue item))

(defn empty-queue?
  [queue]
  (nil? (front-pointer queue)))

(defn make-queue
  []
  (cons-pair nil nil))

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

