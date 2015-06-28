(ns sicp.test.chapter-2.henderson-escher
  (:use sicp.chapter-2.henderson-escher)
  (:use clojure.test))

;vectors

(deftest can-scale-vectors
  (is (= (scale-vect 3 (make-vect 2 1)) (make-vect 6 3))))

(deftest can-add-vectors
  (is (= (add-vect (make-vect 3 1) (make-vect 2 1)) (make-vect 5 2))))

(deftest can-subtract-vectors
  (is (= (sub-vect (make-vect 3 1) (make-vect 2 1)) (make-vect 1 0))))

;frames

(defn test-frame-model[constructor origin-selector edge1-selector edge2-selector]
  (let [origin (make-vect 3 1)
        edge1 (make-vect 2 4)
        edge2 (make-vect 1 4)
        frame (constructor origin edge1 edge2)]
    (is (= (origin-selector frame) origin))
    (is (= (edge1-selector frame) edge1))
    (is (= (edge2-selector frame) edge2))))

(deftest list-frame-model-correct
  (test-frame-model make-frame origin-frame edge1-frame edge2-frame))

(deftest pairs-frame-model-correct
  (test-frame-model make-frame-pair origin-frame-pair edge1-frame-pair edge2-frame-pair))
