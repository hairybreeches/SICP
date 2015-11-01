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
    (delete-queue! queue)
    (is (= (front-queue queue) :b))
    (delete-queue! queue)
    (is (= (front-queue queue) :c))
    (delete-queue! queue)
    (is (= (front-queue queue) :d))
    (insert-queue! queue :e)
    (is (= (front-queue queue) :d))
    (delete-queue! queue)
    (is (= (front-queue queue) :e))
    (delete-queue! queue)
    (is (= (front-queue queue) nil))))
