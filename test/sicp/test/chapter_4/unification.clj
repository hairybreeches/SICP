(ns sicp.test.chapter-4.unification
  (:use clojure.test)
  (:use sicp.chapter-4.logic.unification)
  (:use sicp.chapter-4.logic.frames))

(defn- add-value
  [frame key-value-pair]
  (extend-frame (first key-value-pair) (second key-value-pair) frame))

(defn make-frame
  [key-value-map]
  (reduce add-value (create-empty-frame) key-value-map))

(deftest can-unify-frames-with-distinct-values
  (is (=
        (make-frame {:x 12 :y 10})
        (unify-frames
          (make-frame {:x 12})
          (make-frame {:y 10})))))

(deftest can-unify-frames-with-same-values
  (is (=
        (make-frame {:x 12 :y 10})
        (unify-frames
          (make-frame {:x 12 :y 10})
          (make-frame {:x 12 :y 10})))))
