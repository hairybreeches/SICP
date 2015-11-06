(ns sicp.test.chapter-3.queues
  (:use sicp.chapter-3.queues)
  (:use clojure.test))


(deftest can-add-and-remove-elements-to-queue
  (let [queue (-> (make-queue)
                   (insert-queue! :a)
                   (insert-queue! :b)
                   (insert-queue! :c)
                   (insert-queue! :d))]
    (is (= (front-queue queue) :a))
    (is (not (empty-queue? queue)))

    (delete-queue! queue)
    (is (= (front-queue queue) :b))
    (is (not (empty-queue? queue)))

    (delete-queue! queue)
    (is (= (front-queue queue) :c))
    (is (not (empty-queue? queue)))

    (delete-queue! queue)
    (is (= (front-queue queue) :d))
    (is (not (empty-queue? queue)))

    (insert-queue! queue :e)
    (is (= (front-queue queue) :d))
    (is (not (empty-queue? queue)))

    (delete-queue! queue)
    (is (= (front-queue queue) :e))
    (is (not (empty-queue? queue)))

    (delete-queue! queue)
    (is (= (front-queue queue) nil))
    (is (empty-queue? queue))))

(deftest can-add-and-remove-elements-to-deque
  (let [deque (-> (make-deque)
                   (rear-insert-deque! :b)
                   (rear-insert-deque! :c)
                   (rear-insert-deque! :d)
                   (front-insert-deque! :a))]

    (is (= (deque->list deque) '(:a :b :c :d)))
    (is (= (front-deque deque) :a))
    (is (= (rear-deque deque) :d))
    (is (not (empty-deque? deque)))

    (front-delete-deque! deque)
    (is (= (deque->list deque) '(:b :c :d)))
    (is (= (front-deque deque) :b))
    (is (= (rear-deque deque) :d))
    (is (not (empty-deque? deque)))

    (rear-delete-deque! deque)
    (is (= (deque->list deque) '(:b :c)))
    (is (= (front-deque deque) :b))
    (is (= (rear-deque deque) :c))
    (is (not (empty-deque? deque)))

    (front-insert-deque! deque :g)
    (is (= (deque->list deque) '(:g :b :c)))
    (is (= (front-deque deque) :g))
    (is (= (rear-deque deque) :c))
    (is (not (empty-deque? deque)))

    (front-delete-deque! deque)
    (is (= (deque->list deque) '(:b :c)))
    (is (= (front-deque deque) :b))
    (is (= (rear-deque deque) :c))
    (is (not (empty-deque? deque)))

    (front-delete-deque! deque)
    (is (= (deque->list deque) '(:c)))
    (is (= (front-deque deque) :c))
    (is (= (rear-deque deque) :c))
    (is (not (empty-deque? deque)))

    (rear-insert-deque! deque :e)
    (is (= (deque->list deque) '(:c :e)))
    (is (= (front-deque deque) :c))
    (is (= (rear-deque deque) :e))
    (is (not (empty-deque? deque)))

    (front-delete-deque! deque)
    (is (= (deque->list deque) '(:e)))
    (is (= (front-deque deque) :e))
    (is (= (rear-deque deque) :e))
    (is (not (empty-deque? deque)))

    (front-delete-deque! deque)
    (is (= (deque->list deque) '()))
    (is (= (front-deque deque) nil))
    (is (= (rear-deque deque) nil))
    (is (empty-deque? deque))

    (front-insert-deque! deque :f)
    (is (= (deque->list deque) '(:f)))
    (is (= (front-deque deque) :f))
    (is (= (rear-deque deque) :f))
    (is (not (empty-deque? deque)))

    (rear-delete-deque! deque)
    (is (= (deque->list deque) '()))
    (is (= (front-deque deque) nil))
    (is (= (rear-deque deque) nil))
    (is (empty-deque? deque))))
