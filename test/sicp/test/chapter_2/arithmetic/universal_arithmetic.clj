(ns sicp.test.chapter-2.arithmetic.universal-arithmetic
  (:require sicp.chapter-2.arithmetic.universal-arithmetic)
  (:use sicp.chapter-2.arithmetic.arithmetic-operations)
  (:use sicp.chapter-2.arithmetic.elements.rational-numbers)
  (:use sicp.chapter-2.arithmetic.elements.complex-numbers)
  (:use clojure.test))

(deftest can-add-integers
  (is (= (add 2 3) 5)))

(deftest can-subtract-integers
  (is (= (sub 2 3) -1)))

(deftest can-multiply-integers
  (is (= (mul 2 3) 6)))

(deftest can-divide-integers
  (is (= (div 6 3) 2)))

(deftest can-add-rationals
  (is (= (add (make-rat 1 2) (make-rat 2 3)) (make-rat 7 6))))

(deftest can-subtract-rationals
  (is (= (sub (make-rat 1 2) (make-rat 2 3)) (make-rat -1 6))))

(deftest can-multiply-rationals
  (is (= (mul (make-rat 1 2) (make-rat 2 3)) (make-rat 1 3))))

(deftest can-divide-rationals
  (is (= (div (make-rat 1 2) (make-rat 2 3)) (make-rat 3 4))))

(deftest can-add-two-real-imag-numbers
  (is (= (add (make-from-real-imag 2 5) (make-from-real-imag -3 8)) (make-from-real-imag -1 13))))

(deftest can-subtract-two-real-imag-numbers
  (is (equ? (sub (make-from-real-imag 2 5) (make-from-real-imag -3 8)) (make-from-real-imag 5 -3))))

(deftest can-multiply-two-mag-angle-numbers
  (is (= (mul (make-from-mag-ang 2 5) (make-from-mag-ang -3 8)) (make-from-mag-ang -6 13))))

(deftest can-divide-two-mag-angle-numbers
  (is (= (div (make-from-mag-ang 4 2) (make-from-mag-ang 2 4)) (make-from-mag-ang 2 -2))))

(deftest can-determine-equality-of-integers
  (is (equ? 3 3)))

(deftest can-determine-inequality-of-integers
  (is (not (equ? 3 4))))

(deftest can-determine-equality-of-rationals
  (is (equ? (make-rat 3 4) (make-rat -6 -8))))

(deftest can-determine-inequality-of-rationals
  (is (not (equ? (make-rat 3 4) (make-rat 5 8)))))

(deftest can-determine-equality-of-complex
  (is (equ? (make-from-real-imag 3 4) (make-from-real-imag 3 4))))

(deftest can-determine-inequality-of-complex
  (is (not (equ? (make-from-real-imag 3 4) (make-from-real-imag 3 2)))))

(deftest can-determine-zeroness-of-integers
  (is (nought? 0)))

(deftest can-determine-non-zeroness-of-integers
  (is (not (nought? 3))))

(deftest can-determine-zeroness-of-rationals
  (is (nought? (make-rat 0 4))))

(deftest can-determine-non-zeroness-of-rationals
  (is (not (nought? (make-rat 3 4)))))

(deftest can-determine-zeroness-of-complex
  (is (nought? (make-from-real-imag 0 0))))

(deftest can-determine-non-zeroness-of-complex
  (is (not (nought? (make-from-real-imag 3 0)))))

(deftest can-add-rational-and-int
  (is (= (add (make-rat 3 4) 1) (make-rat 7 4))))

(deftest can-subtract-complex-and-rational
  (is (= (sub (make-rat 3 4) (make-from-real-imag 1 -2)) (make-from-real-imag -0.25 2))))

(deftest projects-when-possible
  (is (= (mul (make-from-real-imag 3 4) (make-from-real-imag 3 -4)) 25)))

(deftest stops-projecting
  (is (= (mul (make-from-real-imag 0.5 0.5) (make-from-real-imag 0.5 -0.5)) (make-rat 1 2))))


(deftest can-add-complex-numbers-with-rational-coefficients
  (let [add-1 (make-from-real-imag (make-rat 1 2) (make-rat 4 5))
        add-2 (make-from-real-imag (make-rat 3 2) 1)
        result (make-from-real-imag 2 (make-rat 9 5))]
      (is (= (add add-1 add-2) result))))

(deftest can-multiply-complex-numbers-with-rational-coefficients
  (let [mul-1 (make-from-real-imag (make-rat 1 5) (make-rat 1 2))
        mul-2 (make-from-real-imag (make-rat 1 5) (make-rat -1 2))
        result (mul mul-1 mul-2)]
      (is (= (make-rat 29 100) result))))


