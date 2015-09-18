(ns sicp.test.chapter-2.arithmetic.complex-numbers
  (:use sicp.chapter-2.arithmetic.complex-numbers)
  (:use clojure.math.numeric-tower)
  (:use clojure.test)
  (:require sicp.chapter-2.arithmetic.universal-arithmetic)
  (:use sicp.chapter-2.arithmetic.arithmetic-operations)
  (:use sicp.test.assertions))

(def pi java.lang.Math/PI)

(defn is-complex=[a b]
    (is (roughly= (real-part a) (real-part b) 8))
    (is (roughly= (imag-part a) (imag-part b) 8)))

(deftest atan-quadrants-correct
  (is (= (angle (make-from-real-imag 1 1)) (/ pi 4)))
  (is (= (angle (make-from-real-imag -1 1)) (* pi 3/4)))
  (is (= (angle (make-from-real-imag -1 -1)) (* pi -3/4)))
  (is (= (angle (make-from-real-imag 1 -1)) (* pi -1/4))))

(deftest can-add-two-real-imag-numbers
  (is-complex= (add-complex (make-from-real-imag 2 5) (make-from-real-imag -3 8)) (make-from-real-imag -1 13)))

(deftest can-subtract-two-real-imag-numbers
  (is-complex= (sub (make-from-real-imag 2 5) (make-from-real-imag -3 8)) (make-from-real-imag 5 -3)))

(deftest can-multiply-two-real-imag-numbers
  (is-complex= (mul-complex (make-from-real-imag 2 5) (make-from-real-imag -3 8)) (make-from-real-imag -46 1)))

(deftest can-divide-two-real-imag-numbers
  (is-complex= (div-complex (make-from-real-imag 50 25) (make-from-real-imag 3 -4)) (make-from-real-imag 2 11)))

(deftest can-add-two-mag-angle-numbers
  (is-complex= (add-complex (make-from-mag-ang 2 (/ pi 3)) (make-from-mag-ang 2 (/ pi 6))) (make-from-real-imag (+ 1 (sqrt 3)) (+ 1 (sqrt 3)))))

(deftest can-subtract-two-mag-angle-numbers
  (is-complex= (sub  (make-from-mag-ang 2 (/ pi 3)) (make-from-mag-ang 2 (/ pi 6))) (make-from-real-imag (- 1 (sqrt 3)) (- (sqrt 3) 1))))

(deftest can-multiply-two-mag-angle-numbers
  (is-complex= (mul-complex (make-from-mag-ang 2 5) (make-from-mag-ang -3 8)) (make-from-mag-ang -6 13)))

(deftest can-divide-two-mag-angle-numbers
  (is-complex= (div-complex (make-from-mag-ang 4 2) (make-from-mag-ang 2 4)) (make-from-mag-ang 2 -2)))

(deftest can-add-mixed-numbers
  (is-complex= (add-complex (make-from-real-imag 2 5) (make-from-mag-ang (sqrt 2) (/ pi 4))) (make-from-real-imag 3 6)))

(deftest can-subtract-mixed-numbers
  (is-complex= (sub (make-from-mag-ang (sqrt 2) (/ pi 4)) (make-from-real-imag -3 8)) (make-from-real-imag 4 -7)))

(deftest can-multiply-mixed-numbers
  (is-complex= (mul-complex (make-from-real-imag 2 5) (make-from-mag-ang (sqrt 2) (/ pi 4))) (make-from-real-imag -3 7)))

(deftest can-divide-mixed-numbers
  (is-complex= (div-complex (make-from-real-imag 4 -8) (make-from-mag-ang (sqrt 2) (/ pi 4))) (make-from-real-imag -2 -6)))


