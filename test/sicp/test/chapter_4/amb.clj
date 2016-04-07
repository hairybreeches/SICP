(ns sicp.test.chapter-4.amb
  (:use sicp.chapter-4.interpreter.repl)
  (:use sicp.chapter-4.amb-code)
  (:use clojure.test))

(deftest amb-returns-results
  (is
    (= (get-all-results '(amb 1 2 3))
      '(1 2 3))))

(deftest pythagorean-triples
  (is
    (=
      (get-pythagorean-triples-between 1 13)
      '((3 4 5) (5 12 13) (6 8 10)))))

(deftest infinite-pythagorean-triples
  (is
    (=
      (take 4 (get-pythagorean-triples))
      '((3 4 5) (6 8 10) (5 12 13) (9 12 15)))))

(deftest multiple-dwelling-amb
  (is (=
        (get-multiple-dwelling-solutions)
        '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))))))

(deftest liars
  (is (= (get-liars-solutions)
        '(((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))))))

(deftest yachts
  (is (= (get-yachts-solutions)
         '((melissa hood mary hall lorna parker rosalind downing gabrielle moore)
           (melissa hood mary moore lorna downing rosalind parker gabrielle hall)))))

(deftest queens
  (is (= (take 5 (get-queens-solutions))
         '(((8 4) (7 2) (6 7) (5 3) (4 6) (3 8) (2 5) (1 1))
           ((8 5) (7 2) (6 4) (5 7) (4 3) (3 8) (2 6) (1 1))
           ((8 3) (7 5) (6 2) (5 8) (4 6) (3 4) (2 7) (1 1))
           ((8 3) (7 6) (6 4) (5 2) (4 8) (3 5) (2 7) (1 1))
           ((8 5) (7 7) (6 1) (5 3) (4 8) (3 6) (2 4) (1 2))))))












