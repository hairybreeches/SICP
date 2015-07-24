(ns sicp.test.chapter-2.symbolic-data
  (:use sicp.chapter-2.symbolic-data)
  (:use clojure.test))

(deftest sequence-equal-empty
  (is (sequence-equal '() [])))

(deftest sequence-equal-nil-and-empty
  (is (sequence-equal '() nil)))

(deftest same-sequences-equal
  (is (sequence-equal '(1 2 3) [1 2 3])))

(deftest subsequence-not-equal
  (is (not (sequence-equal '(1 2 3 4 5) [1 2 3]))))


(deftest different-sequences-not-equal
  (is (not (sequence-equal '(1 2 3 4 5) '(1 2 3 4 6)))))

(deftest deriv-sum
  (is (= (deriv '(+ x 3) 'x) 1)))

(deftest deriv-multi
  (is (= (deriv '(* x y) 'x) 'y)))

(deftest deriv-product-and-sum
  (is (= (deriv '(* (* x y) (+ x 3)) 'x) '(+ (* x y)
                                             (* y (+ x 3))))))

(deftest deriv-polynomial
  (is (= (deriv '(+ (* (** x 3) y) (* 4 (** x 2))) 'x)
         '(+ (* (* 3 (** x 2)) y) (* 4 (* 2 x))))))

(deftest deriv-exponent-zero
  (is (= (deriv (deriv '(+ (** y 3) (** x 2)) 'x) 'x) 2)))


(deftest deriv-multi-sum
  (is (= (deriv '(+ (+ (* 2 x) (* y x)) (* z x)) 'x) '(+ 2 y z))))
