(ns sicp.test.chapter-2.arithmetic.polynomials
  (:use sicp.chapter-2.arithmetic.elements.polynomials)
  (:require sicp.chapter-2.arithmetic.universal-arithmetic)
  (:use sicp.chapter-2.arithmetic.elements.rational-numbers)
  (:use sicp.chapter-2.arithmetic.elements.complex-numbers)
  (:use sicp.chapter-2.arithmetic.arithmetic-operations)
  (:use sicp.chapter-2.arithmetic.numerical-type-system)
  (:use clojure.test))
;sparse tests
(def linear-poly-in-y
  (make-poly
     'y
     (make-sparse-termlist (make-term 1 1)
                           (make-term 0 6))))

(def double-linear-poly-in-y
    (make-poly
     'y
     (make-sparse-termlist (make-term 1 2)
                           (make-term 0 12))))

(def quadratic-1
  (make-poly
     'x
     (make-sparse-termlist (make-term 2 1)
                    (make-term 1 -5)
                    (make-term 0 6))))

(def quadratic-2
  (make-poly
     'x
     (make-sparse-termlist (make-term 2 1)
                    (make-term 1 -4)
                    (make-term 0 4))))

(def linear-1
  (make-poly
     'x
     (make-sparse-termlist (make-term 1 1)
                    (make-term 0 -2))))

(def linear-2
  (make-poly
     'x
     (make-sparse-termlist (make-term 1 1)
                    (make-term 0 -3))))


(def linear-1
  (make-poly
     'x
     (make-sparse-termlist (make-term 1 1)
                    (make-term 0 -2))))

(def linear-2
  (make-poly
     'x
     (make-sparse-termlist (make-term 1 1)
                    (make-term 0 -3))))


(def linear-rat
  (make-poly
     'x
     (make-sparse-termlist (make-term 1 (make-rat 3 4))
                    (make-term 0 (make-rat 4 5)))))

(def quadratic-rat
  (make-poly
     'x
     (make-sparse-termlist (make-term 2 1)
                    (make-term 1 (make-rat 8 3))
                    (make-term 0 (make-rat 1 2)))))

(def cubic-rat
  (make-poly
     'x
     (make-sparse-termlist (make-term 3 (make-rat 3 4))
                    (make-term 2 (make-rat 14 5))
                    (make-term 1 (make-rat 301 120))
                    (make-term 0 (make-rat 2 5)))))

(def multivariate-1
  (make-poly
   'x
   (make-sparse-termlist (make-term 2 (make-poly 'y (make-sparse-termlist (make-term 1 1) (make-term 0 1))))
                  (make-term 1 (make-poly 'y (make-sparse-termlist (make-term 2 1) (make-term 0 1))))
                  (make-term 0 (make-poly 'y (make-sparse-termlist (make-term 1 1) (make-term 0 -1)))))))

(def multivariate-2
  (make-poly
   'x
   (make-sparse-termlist (make-term 1 (make-poly 'y (make-sparse-termlist (make-term 1 1) (make-term 0 -2))))
                  (make-term 0 (make-poly 'y (make-sparse-termlist (make-term 3 1) (make-term 0 7)))))))

(def multivariate-3
  (make-poly
   'x
   (make-sparse-termlist (make-term 3 (make-poly 'y (make-sparse-termlist (make-term 2 1) (make-term 1 -1) (make-term 0 -2))))
                  (make-term 2 (make-poly 'y (make-sparse-termlist (make-term 4 1) (make-term 3 2) (make-term 2 -2) (make-term 1 8) (make-term 0 5))))
                  (make-term 1 (make-poly 'y (make-sparse-termlist (make-term 5 1) (make-term 3 1) (make-term 2 8) (make-term 1 -3) (make-term 0 9))))
                  (make-term 0 (make-poly 'y (make-sparse-termlist (make-term 4 1) (make-term 3 -1) (make-term 1 7) (make-term 0 -7)))))))

(deftest can-mul-polynomial-by-constant
  (is (= (mul linear-poly-in-y 2) double-linear-poly-in-y)))

(deftest can-mul-polynomials
  (is (= quadratic-1 (mul-poly linear-1 linear-2))))

(deftest can-add-polynomials
  (is (= quadratic-2 (add-poly linear-1 quadratic-1))))

(deftest can-sub-polynomials
  (is (= linear-1 (sub quadratic-2 quadratic-1))))

(deftest can-mul-rat-polynomials
  (is (= cubic-rat (mul-poly quadratic-rat linear-rat))))

(deftest can-mul-multivariate
  (is (= multivariate-3 (mul-poly multivariate-1 multivariate-2))))

(deftest can-project-to-last-coefficient
  (is (= (number-project multivariate-1) (make-poly 'y (make-sparse-termlist (make-term 1 1) (make-term 0 -1))))))

(deftest can-subtract-to-zero
  (is (= (sub linear-1 linear-1) 0)))

(deftest can-compare-to-constant
  (is (equ? (make-poly 'x (make-sparse-termlist (make-term 0 2))) 2)))

(deftest can-compare-to-zero
  (is (equ? (make-poly 'x (make-sparse-termlist)) 0)))

(deftest can-compare-to-constant-when-non-x-polynomial
  (is (equ? (make-poly 'e (make-sparse-termlist (make-term 0 2))) 2)))

(deftest can-simplify-to-zero
  (is (= (simplify (make-poly 'x (make-sparse-termlist))) 0)))

(deftest can-simplify-to-constant-integer
  (is (= (simplify (make-poly 'x (make-sparse-termlist (make-term 0 4)))) 4)))

(deftest can-simplify-to-constant-complex
  (is (= (simplify (make-poly 'x (make-sparse-termlist (make-term 0 (make-from-real-imag 2 3))))) (make-from-real-imag 2 3))))

;dense tests
(def quadratic-1-dense
  (make-poly
     'x
     (make-dense-termlist 1 -5 6)))

(def quadratic-2-dense
  (make-poly
     'x
     (make-dense-termlist 1 -4 4)))

(def linear-1-dense
  (make-poly
     'x
     (make-dense-termlist 1 -2)))

(def linear-2-dense
  (make-poly
     'x
     (make-dense-termlist 1 -3)))

(def linear-1-dense
  (make-poly
     'x
     (make-dense-termlist 1 -2)))

(def linear-2-dense
  (make-poly
     'x
     (make-dense-termlist 1 -3)))


(def linear-rat-dense
  (make-poly
     'x
     (make-dense-termlist (make-rat 3 4) (make-rat 4 5))))

(def quadratic-rat-dense
  (make-poly
     'x
     (make-dense-termlist 1 (make-rat 8 3) (make-rat 1 2))))

(def cubic-rat-dense
  (make-poly
     'x
    (make-dense-termlist (make-rat 3 4)
                    (make-rat 14 5)
                    (make-rat 301 120)
                    (make-rat 2 5))))

(def multivariate-1-dense
  (make-poly
   'x
   (make-dense-termlist (make-poly 'y (make-sparse-termlist (make-term 1 1) (make-term 0 1)))
                  (make-poly 'y (make-sparse-termlist (make-term 2 1) (make-term 0 1)))
                  (make-poly 'y (make-sparse-termlist (make-term 1 1) (make-term 0 -1))))))

(def multivariate-2-dense
  (make-poly
   'x
   (make-dense-termlist (make-poly 'y (make-sparse-termlist (make-term 1 1) (make-term 0 -2)))
                  (make-poly 'y (make-sparse-termlist (make-term 3 1) (make-term 0 7))))))

(def multivariate-3-dense
  (make-poly
   'x
   (make-dense-termlist (make-poly 'y (make-sparse-termlist (make-term 2 1) (make-term 1 -1) (make-term 0 -2)))
                  (make-poly 'y (make-sparse-termlist (make-term 4 1) (make-term 3 2) (make-term 2 -2) (make-term 1 8) (make-term 0 5)))
                  (make-poly 'y (make-sparse-termlist (make-term 5 1) (make-term 3 1) (make-term 2 8) (make-term 1 -3) (make-term 0 9)))
                  (make-poly 'y (make-sparse-termlist (make-term 4 1) (make-term 3 -1) (make-term 1 7) (make-term 0 -7))))))

(deftest can-mul-polynomials-dense
  (is (= quadratic-1-dense (mul-poly linear-1-dense linear-2-dense))))

(deftest can-add-polynomials-dense
  (is (= quadratic-2-dense (add-poly linear-1-dense quadratic-1-dense))))

(deftest can-sub-polynomials-dense
  (is (= linear-1-dense (sub quadratic-2-dense quadratic-1-dense))))

(deftest can-mul-rat-polynomials-dense
  (is (= cubic-rat-dense (mul-poly quadratic-rat-dense linear-rat-dense))))

(deftest can-mul-multivariate-dense
  (is (= multivariate-3-dense (mul-poly multivariate-1-dense multivariate-2-dense))))

(deftest can-project-to-last-coefficient-dense
  (is (= (number-project multivariate-1-dense) (make-poly 'y (make-sparse-termlist (make-term 1 1) (make-term 0 -1))))))

(deftest can-compare-to-constant-dense
  (is (equ? (make-poly 'x (make-dense-termlist 4)) 4)))

(deftest can-compare-to-zero-dense
  (is (equ? (make-poly 'x (make-dense-termlist)) 0)))

(deftest can-simplify-to-constant-integer-dense
  (is (= (simplify (make-poly 'x (make-dense-termlist 4))) 4)))

(deftest can-simplify-to-zero-dense
  (is (= (simplify (make-poly 'x (make-dense-termlist))) 0)))

(deftest can-simplify-to-constant-complex-dense
  (is (= (simplify (make-poly 'x (make-dense-termlist (make-from-real-imag 2 3)))) (make-from-real-imag 2 3))))

(deftest can-subtract-to-zero-dense
  (is (= (sub linear-1-dense linear-1-dense) 0)))

(deftest can-compare-dense
  (is (equ? (make-poly 'x (make-dense-termlist 2 3))
            (make-poly 'x (make-dense-termlist 2 3)))))

;conversion tests
(defn convert-polynomial-to-sparse[poly]
  (make-poly (variable poly) (to-sparse-format (term-list poly))))

(deftest can-convert-dense-to-sparse-quadratic
  (is (= quadratic-1 (convert-polynomial-to-sparse quadratic-1-dense))))

(deftest can-convert-dense-to-sparse-linear
  (is (= linear-1 (convert-polynomial-to-sparse linear-1-dense))))

(deftest can-convert-dense-to-sparse-rational
  (is (= quadratic-1 (convert-polynomial-to-sparse quadratic-1-dense))))

(deftest can-convert-dense-to-sparse-rational
  (is (= multivariate-3 (convert-polynomial-to-sparse multivariate-3-dense))))

;mixed tests
(deftest can-mul-polynomials-mixed
  (is (= quadratic-1 (mul-poly linear-1 linear-2-dense))))

(deftest can-add-polynomials-mixed
  (is (= quadratic-2 (add-poly linear-1-dense quadratic-1))))

(deftest can-sub-polynomials-mixed
  (is (= linear-1 (sub quadratic-2 quadratic-1-dense))))

(deftest can-mul-rat-polynomials-mixed
  (is (= cubic-rat (mul-poly quadratic-rat-dense linear-rat))))

(deftest can-mul-multivariate-mixed
  (is (= multivariate-3 (mul-poly multivariate-1 multivariate-2-dense))))

;division tests
(deftest can-divide-polynomials-with-no-remainder
  (is (= [linear-1 (make-poly 'x (make-dense-termlist))] (div-poly quadratic-1 linear-2) )))

(deftest can-divide-rat-polynomials-with-no-remainder
  (is (= [linear-rat (make-poly 'x (make-dense-termlist))] (div-poly cubic-rat quadratic-rat))))

(deftest can-divide-polynomials-with-remainder
  (is (= [quadratic-2 linear-2] (div-poly (add
                                          (mul quadratic-1 quadratic-2)
                                          linear-2)
                                       quadratic-1))))

(deftest can-divide-rat-polynomials-with-remainder
  (is (= [linear-rat linear-rat] (div-poly (add cubic-rat linear-rat) quadratic-rat))))

(deftest can-divide-with-constant-remainder
  (is (= [(make-poly 'a (make-sparse-termlist (make-term 0 1)))
          (make-poly 'a (make-sparse-termlist (make-term 0 -1)))]
         (div-poly (make-poly 'a (make-dense-termlist 1 -1))
                   (make-poly 'a (make-dense-termlist 1 0))))))

;variable retrieval
(deftest can-get-variable-from-dense-polynomial
  (is (= '(x) (get-variables linear-rat-dense))))

(deftest can-get-variable-from-sparse-polynomial
  (is (= '(x) (get-variables quadratic-2))))

(deftest can-get-variables-from-sparse-multivariate-polynomial
  (is (= '(x y) (get-variables multivariate-2))))

(deftest can-get-variables-from-dense-multivariate-polynomial
  (is (= '(x y) (get-variables multivariate-1-dense))))

;multivariate tests
(deftest cannot-create-polynomials-in-y-with-coefficients-in-x
  (is (thrown-with-msg? Exception #"Cannot create a polynomial in y with coefficients polynomials in \(x\)" (make-poly 'y (make-dense-termlist linear-1-dense quadratic-2)))))

(def polynomial-in-y (make-poly 'y (make-dense-termlist 4 0 2 1)))

(def multivariate-polynomial
  (make-poly 'x
             (make-dense-termlist
                (make-poly 'y (make-dense-termlist 3 0 (make-rat 3 2) (make-rat 3 4)))
                (make-poly 'y (make-dense-termlist (make-rat 56 5) 0 (make-rat 28 5) (make-rat 14 5)))
                (make-poly 'y (make-dense-termlist (make-rat 6 5) 0 (make-rat 3 5) (make-rat 3 10)))
                (make-poly 'y (make-dense-termlist (make-rat 8 5) 0 (make-rat 4 5) (make-rat 2 5))))))

(def polynomial-in-x
  (make-poly
     'x
    (make-dense-termlist (make-rat 3 4)
                    (make-rat 14 5)
                    (make-rat 3 10)
                    (make-rat 2 5))))

(deftest can-multiply-to-make-multivariate
  (is (= (mul polynomial-in-y polynomial-in-x) multivariate-polynomial)))

(deftest can-subtract-last-term-from-multivariate
  (is (= (sub multivariate-polynomial (make-poly 'y (make-dense-termlist (make-rat 8 5) 0 (make-rat 4 5) (make-rat 2 5)))) (make-poly 'x
             (make-dense-termlist
                (make-poly 'y (make-dense-termlist 3 0 (make-rat 3 2) (make-rat 3 4)))
                (make-poly 'y (make-dense-termlist (make-rat 56 5) 0 (make-rat 28 5) (make-rat 14 5)))
                (make-poly 'y (make-dense-termlist (make-rat 6 5) 0 (make-rat 3 5) (make-rat 3 10)))
                0)))))

;gcd tests
(def p1 (make-poly 'x (make-sparse-termlist (make-term 4 1) (make-term 3 -1) (make-term 2 -2) (make-term 1 2))))
(def p2 (make-poly 'x (make-sparse-termlist (make-term 3 1) (make-term 1 -1))))

(deftest can-calculate-gcd
  (is (= (greatest-common-divisor p1 p2) (make-poly 'x (make-sparse-termlist (make-term 2 -1) (make-term 1 1))))))

(def P1 (make-poly 'x (make-sparse-termlist (make-term 2 1) (make-term 1 -2) (make-term 0 1))))

(def P2 (make-poly 'x (make-dense-termlist 11 0 7)))

(def P3 (make-poly 'x (make-dense-termlist 13 5)))

(def Q1 (mul P1 P2))

(def Q2 (mul P1 P3))

(deftest can-avoid-quotients-when-computing-gcd
  (is (= P1 (greatest-common-divisor Q1 Q2))))

;rational polynomial tests
(def numer-1 (make-poly 'z (make-dense-termlist 1 -1)))
(def denom-1 (make-poly 'z (make-dense-termlist 1 0 0 -1)))

(def numer-2 (make-poly 'z (make-dense-termlist 1 0)))
(def denom-2 (make-poly 'z (make-dense-termlist 1 0 -1)))

(deftest can-make-rational-polynomial-with-reducing
  (let [quotient (make-rat numer-1 denom-1)]
    (is (= (numer quotient) (make-poly 'z (make-sparse-termlist (make-term 0 1)))))
    (is (= (denom quotient) (make-poly 'z (make-sparse-termlist (make-term 2 1) (make-term 1 1) (make-term 0 1)))))))

(deftest can-make-rational-polynomial-where-no-reducing-necessary
  (let [quotient (make-rat numer-2 denom-2)]
    (is (= (numer quotient) (make-poly 'z (make-sparse-termlist (make-term 1 1)))))
    (is (= (denom quotient) (make-poly 'z (make-sparse-termlist (make-term 2 1) (make-term 0 -1)))))))













