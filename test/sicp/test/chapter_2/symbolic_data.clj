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
  (is (= (deriv '(* x y) 'x) '(+ (* x 0) (* 1 y)))))

(deftest deriv-polynomial
  (is (= (deriv '(* (* x y) (+ x 3)) 'x) '(+ (* (* x y) 1)
                                             (* (+ (* x 0) (* 1 y))
                                                (+ x 3))))))
