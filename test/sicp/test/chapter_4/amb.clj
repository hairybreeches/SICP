(ns sicp.test.chapter-4.amb
  (:use sicp.chapter-4.interpreter.repl)
  (:use sicp.chapter-4.amb-code)
  (:use sicp.chapter-4.language)
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
  (is (= (first (get-queens-solutions))
         '((8 4) (7 2) (6 7) (5 3) (4 6) (3 8) (2 5) (1 1)))))

(deftest simple-sentence
  (is (= (parse ''(the cat eats))
         '((sentence (simple-noun-phrase (article the) (noun cat)) (verb eats))))))

(deftest intermediate-sentence
  (is (= (parse ''(the professor lectures to the student with the cat))
         '(
            ;the professor lectures with the cat
            (sentence
              (simple-noun-phrase (article the) (noun professor))
              (verb-phrase
                (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
                (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

            ;the student has the cat
            (sentence
              (simple-noun-phrase (article the) (noun professor))
              (verb-phrase
                (verb lectures)
                (prep-phrase
                  (prep to)
                  (noun-phrase (simple-noun-phrase (article the) (noun student))
                               (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))))))

(deftest complex-sentence
  (is (= (parse ''(the professor lectures to the student in the class with the cat))
         '(
            (sentence
              (simple-noun-phrase (article the) (noun professor))
              (verb-phrase
                (verb-phrase
                  (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
                  (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
                (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))


            (sentence
              (simple-noun-phrase (article the) (noun professor))
              (verb-phrase
                (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
                (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (article the) (noun class))
                                                    (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))

            (sentence
              (simple-noun-phrase (article the) (noun professor))
              (verb-phrase
                (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student))
                                                                                 (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))))
                (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

            (sentence
              (simple-noun-phrase (article the) (noun professor))
              (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun student))
                                                                                            (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
                                                                               (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))

            (sentence
              (simple-noun-phrase (article the) (noun professor))
              (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student))
                                                                               (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (article the) (noun class))
                                                                                                                   (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))))))))












