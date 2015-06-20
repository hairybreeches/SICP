(ns sicp.test.chapter-2.sequences
  (:use sicp.chapter-2.sequences)
  (:use clojure.test))


(deftest last-item-in-single-element-sequence
  (is (= :a (last-pair [:a]))))

(deftest last-item-in-several-element-sequence
  (is (= 6 (last-pair '(1 2 3 4 5 6)))))

(deftest reverse-empty-sequence
  (is (empty? (deep-reverse '()))))

(deftest reverse-single-element-sequence
  (is (= '(:zi) (deep-reverse '(:zi)))))

(deftest reverse-nil
  (is (= nil (deep-reverse nil))))

(deftest test-deep-reverse
  (is (= '(((23 (22) (21 20 19)) (18 17 16) 15 14 13) 12 (11 10 9 (8 7 6)) (5 4 3) (2 1) ) (deep-reverse '((1 2) (3 4 5) ((6 7 8) 9 10 11) 12 (13 14 15 (16 17 18) ((19 20 21) (22) 23)))))))

(deftest test-fringe
  (is (= (range 1 24) (fringe '((1 2) (3 4 5) ((6 7 8) 9 10 11) 12 (13 14 15 (16 17 18) ((19 20 21) (22) 23)))))))

(deftest reverse-several-element-sequence
  (is (= '(6 5 4 3 2 1) (deep-reverse '(1 2 3 4 5 6)))))

(deftest count-change-book-example
  (is (= 292 (count-change 100 us-coins))))

(deftest same-parity-odd
  (is (= '(1 3 5 7) (same-parity 1 2 3 4 5 6 7))))

(deftest same-parity-even
  (is (= '(2 4 6) (same-parity 2 3 4 5 6 7))))

(defn test-square-list[function]
  (is (= '(1 4 9 16) (function '(1 2 3 4)))))

(deftest test-square-list-primitives
  (test-square-list square-list-primitives))

(deftest test-square-list-map
  (test-square-list square-list-map))

(deftest test-for-each
  (is (= (with-out-str (for-each prn '(1 2 :a "woo!"))) "1\r\n2\r\n:a\r\n\"woo!\"\r\n")))

(deftest test-count-leaves
  (is (= 23 (count-leaves '((1 2) (3 4 5) ((6 7 8) 9 10 11) 12 (13 14 15 (16 17 18) ((19 20 21) (22) 23)))))))

(deftest test-count-leaves-nil
  (is (= 0 (count-leaves nil))))

(deftest test-count-leaves-empty
  (is (= 0 (count-leaves '()))))

(deftest pick-seven-part-one
  (is (= 7 (first (rest (first (rest (rest '(1 3 (5 7) 9)))))))))

(deftest pick-seven-part-two
  (is (= 7 (first (rest (first (rest (first (rest (first (rest (first (rest (first (rest '(1 (2 (3 (4 (5 (6 7)))))))))))))))))))))

(deftest can-calculate-total-weight-of-mobile
  (is (= (total-weight (make-mobile (make-branch 4 (make-mobile (make-branch 2 5) (make-branch 3 4))) (make-branch 3 4))) 13)))

(deftest balanced-mobile-true-case
  (is (balanced? (make-mobile (make-branch 4 (make-mobile (make-branch 4 3) (make-branch 2 6))) (make-branch 6 6)))))

(deftest balanced-mobile-top-branch-fails
  (is (not (balanced? (make-mobile (make-branch 3 (make-mobile (make-branch 4 3) (make-branch 2 6))) (make-branch 6 6))))))

(deftest balanced-mobile-lower-branch-fails
  (is (not (balanced? (make-mobile (make-branch 4 (make-mobile (make-branch 4 3) (make-branch 3 6))) (make-branch 6 6))))))

(defn square-tree-test[square-tree]
  (is (= (square-tree '(1 (2 (3 4) 5) (6 7))) '(1 (4 (9 16) 25) (36 49)))))

(deftest square-tree-primitive-test
  (square-tree-test square-tree-primitive))

(deftest square-tree-map-test
  (square-tree-test square-tree-map))

(deftest subsets-test
  (is (= (subsets '(1 2 3)) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))))

(deftest length-test
  (is (= (length '(1 2 3)) 3)))

(deftest append-test
  (is (= (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))))

(deftest map-impl-test
  (is (= (map-impl square '(1 2 3)) '(1 4 9))))

(deftest evaluate-polynomial
  (is (= (horner-eval 2 '(3 0 4 2)) 35)))

(deftest test-accumulate-n
  (is (= (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) '(22 26 30))))








