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

;doubly-linked items
(defn- make-double-linked-item
  [previous nxt item]
  (cons-pair previous (cons-pair item nxt)))

(defn- get-previous
  [dll-item]
  (car dll-item))

(defn- get-next
  [dll-item]
  (cdr (cdr dll-item)))

(defn- set-previous!
  [dll-item previous]
  (set-car! dll-item previous))

(defn- set-next!
  [dll-item nxt]
  (set-cdr! (cdr dll-item) nxt))

(defn- get-item
  [dll-item]
  (car (cdr dll-item)))

(defn- join!
  [one two]
  (set-next! one two)
  (set-previous! two one))

;double ended queues
(defn make-deque
  []
  (make-queue))

(defn empty-deque?
  [deque]
  (empty-queue? deque))

(defn front-deque
  [deque]
  (if (empty-deque? deque)
      nil
      (get-item (front-pointer deque))))

(defn rear-deque
  [deque]
  (if (empty-deque? deque)
      nil
      (get-item (rear-pointer deque))))

(defn rear-insert-deque!
  [deque item]
  (let [new-item (make-double-linked-item nil nil item)]
    (if (empty-deque? deque)
          (do
            (set-front-pointer! deque new-item)
            (set-rear-pointer! deque new-item)
            deque)
          (do
            (join! (rear-pointer deque) new-item)
            (set-rear-pointer! deque new-item)
            deque))))

(defn front-insert-deque!
  [deque item]
    (if (empty-deque? deque)
          (rear-insert-deque! deque item)
          (let [new-item (make-double-linked-item nil nil item)]
            (join! new-item (front-pointer deque))
            (set-front-pointer! deque new-item)
            deque)))


(defn front-delete-deque!
  [deque]
  (if (empty-deque? deque)
      deque
      (do
        (set-front-pointer! deque (get-next (front-pointer deque)))
        (if (nil? (front-pointer deque))
            (set-rear-pointer! deque nil)
            (set-previous! (front-pointer deque) nil))
        deque)))

(defn rear-delete-deque!
  [deque]
  (if (empty-queue? deque)
      deque
      (do
        (set-rear-pointer! deque (get-previous (rear-pointer deque)))
        (if (nil? (rear-pointer deque))
            (set-front-pointer! deque nil)
            (set-next! (rear-pointer deque) nil))
        deque)))

(defn deque->list
  [deque]
  (loop
    [element (rear-pointer deque)
     so-far '()]
    (if (nil? element)
        so-far
        (recur
          (get-previous element)
          (cons (get-item element) so-far)))))








































