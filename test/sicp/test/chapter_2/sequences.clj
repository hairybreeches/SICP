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

(deftest test-fold-right-n
  (is (= (fold-right-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) '(22 26 30))))

(deftest test-dot-product
  (is (= (dot-product '(1 2 3) '(4 5 6)) 32)))

(deftest test-matrix-*-vector
  (is (= (matrix-*-vector '((1 2 3) (4 5 6) (7 8 9)) '(10 11 12)) '(68 167 266))))

(deftest test-transpose
  (is (= (transpose '((1 2 3) (4 5 6) (7 8 9))) '((1 4 7) (2 5 8) (3 6 9)))))

(deftest test-matrix-*
  (is (= (matrix-* '((1 2) (3 4)) '((5 6) (7 8))) '((19 22) (43 50)))))

;find the rest of my answer to 2.38 here: http://community.schemewiki.org/?sicp-ex-2.38
(deftest exercise-2-38
  (is (= (fold-right / 1 '(1 2 3)) 3/2))
  (is (= (fold-left / 1 '(1 2 3)) 1/6))
  (is (= (fold-right list '() '(1 2 3)) '(1 (2 (3 ())))))
  (is (= (fold-left list '() '(1 2 3)) '(((() 1) 2) 3))))

(deftest test-reverse-foldl
  (is (= (reverse-foldl '(1 2 3 4)) '(4 3 2 1))))

(deftest test-reverse-foldr
  (is (= (reverse-foldr '(1 2 3 4)) '(4 3 2 1))))

(deftest test-unique-pairs
  (is (= (unique-pairs 4) '((2 1) (3 1) (3 2) (4 1) (4 2) (4 3)))))

(deftest test-prime-sum-pairs
  (is (= (prime-sum-pairs 6) '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11)))))

(deftest ordered-triples-max-value-greater-than-sum
  (is (= (ordered-triples 9 6) '((3 2 1)))))

(deftest ordered-triples-sum-0-mod-3
  (is (= (ordered-triples 5 9) '((4 3 2) (5 3 1)))))

(deftest ordered-triples-sum-1-mod-3
  (is (= (ordered-triples 7 10) '((5 3 2) (5 4 1) (6 3 1) (7 2 1)))))

(deftest ordered-triples-sum-2-mod-3
  (is (= (ordered-triples 9 11) '((5 4 2) (6 3 2) (6 4 1) (7 3 1) (8 2 1)))))

(deftest ordered-triples-max-value-restricts
  (is (= (ordered-triples 6 11) '((5 4 2) (6 3 2) (6 4 1)))))

(deftest four-queens-test
  (is (= (queens 4) '(((4 3) (3 1) (2 4) (1 2)) ((4 2) (3 4) (2 1) (1 3))))))









