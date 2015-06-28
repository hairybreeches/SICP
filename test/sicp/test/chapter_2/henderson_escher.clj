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
