(ns sicp.test.chapter-2.arithmetic.polynomials
  (:use sicp.chapter-2.arithmetic.polynomials)
  (:use sicp.chapter-2.arithmetic.rational-numbers)
  (:use clojure.test))

(def quadratic-1
  (make-poly
     'x
     (make-termlist (make-term 2 1)
                    (make-term 1 -5)
                    (make-term 0 6))))

(def quadratic-2
  (make-poly
     'x
     (make-termlist (make-term 2 1)
                    (make-term 1 -4)
                    (make-term 0 4))))

(def linear-1
  (make-poly
     'x
     (make-termlist (make-term 1 1)
                    (make-term 0 -2))))

(def linear-2
  (make-poly
     'x
     (make-termlist (make-term 1 1)
                    (make-term 0 -3))))


(def linear-1
  (make-poly
     'x
     (make-termlist (make-term 1 1)
                    (make-term 0 -2))))

(def linear-2
  (make-poly
     'x
     (make-termlist (make-term 1 1)
                    (make-term 0 -3))))


(def linear-rat
  (make-poly
     'x
     (make-termlist (make-term 1 (make-rat 3 4))
                    (make-term 0 (make-rat 4 5)))))

(def quadratic-rat
  (make-poly
     'x
     (make-termlist (make-term 2 1)
                    (make-term 1 (make-rat 8 3))
                    (make-term 0 (make-rat 1 2)))))

(def cubic-rat
  (make-poly
     'x
     (make-termlist (make-term 3 (make-rat 3 4))
                    (make-term 2 (make-rat 14 5))
                    (make-term 1 (make-rat 301 120))
                    (make-term 0 (make-rat 2 5)))))


(deftest can-mul-polynomials
  (is (= quadratic-1 (mul-poly linear-1 linear-2))))

(deftest can-add-polynomials
  (is (= quadratic-2 (add-poly linear-1 quadratic-1))))

(deftest cab-mul-rat-polynomials
  (is (= cubic-rat (mul-poly quadratic-rat linear-rat))))







