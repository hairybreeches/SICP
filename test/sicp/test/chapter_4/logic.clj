(ns sicp.test.chapter-4.logic
  (:use sicp.chapter-4.logic.sample-database)
  (:use sicp.chapter-4.logic.querier)
  (:use sicp.test.assertions)
  (:use clojure.test))

(defn- returns-results

  ([query expected-results]
   (returns-results people query expected-results))

  ([db query expected-results]
   (is-set= (apply hash-set (execute-query db query))
            (apply hash-set expected-results))))

(deftest retrieve-by-supervisor
  (returns-results
    '(supervisor ?x (Bitdiddle Ben))

    '((supervisor (Hacker Alyssa P) (Bitdiddle Ben))
      (supervisor (Fect Cy D) (Bitdiddle Ben))
      (supervisor (Tweakit Lem E) (Bitdiddle Ben)))))

(deftest retrieve-by-division
  (returns-results
    '(job ?name (accounting . ?job))

    '((job (Scrooge Eben) (accounting chief accountant))
      (job (Cratchet Robert) (accounting scrivener)))))

(deftest retrieve-by-town
  (returns-results
    '(address ?name (Slumerville . ?address))

    '((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
      (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
      (address (Aull DeWitt) (Slumerville (Onion Square) 5)))))

(deftest retrieve-by-supervisor-with-address
  (returns-results
    '(and (supervisor ?name (Bitdiddle Ben))
          (address ?name ?address))

    '((and (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
           (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
      (and (supervisor (Fect Cy D) (Bitdiddle Ben))
           (address (Fect Cy D) (Cambridge (Ames Street) 3)))
      (and (supervisor (Tweakit Lem E) (Bitdiddle Ben))
           (address (Tweakit Lem E) (Boston (Bay State Road) 22))))))

(deftest salary-less-than-bens
  (returns-results
    '(and (salary (Bitdiddle Ben) ?ben-salary)
          (salary ?person ?amount)
          (clojure-value < ?amount ?ben-salary))

    '(
       (and (salary (Bitdiddle Ben) 60000)
            (salary (Hacker Alyssa P) 40000)
            (clojure-value < 40000 60000))

       (and (salary (Bitdiddle Ben) 60000)
            (salary (Fect Cy D) 35000)
            (clojure-value < 35000 60000))

       (and (salary (Bitdiddle Ben) 60000)
            (salary (Tweakit Lem E) 25000)
            (clojure-value < 25000 60000))

       (and (salary (Bitdiddle Ben) 60000)
            (salary (Reasoner Louis) 30000)
            (clojure-value < 30000 60000))

       (and (salary (Bitdiddle Ben) 60000)
            (salary (Cratchet Robert) 18000)
            (clojure-value < 18000 60000))

       (and (salary (Bitdiddle Ben) 60000)
            (salary (Aull DeWitt) 25000)
            (clojure-value < 25000 60000)))))

(deftest non-computer-supervisor
  (returns-results
    '(and (supervisor ?person ?supervisor)
          (not (job ?supervisor (computer . ?title)))
          (job ?supervisor ?role))
    '(
       (and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
            (not (job (Warbucks Oliver) (computer . ?title)))
            (job (Warbucks Oliver) (administration big wheel)))

       (and (supervisor (Scrooge Eben) (Warbucks Oliver))
            (not (job (Warbucks Oliver) (computer . ?title)))
            (job (Warbucks Oliver) (administration big wheel)))

       (and (supervisor (Cratchet Robert) (Scrooge Eben))
            (not (job (Scrooge Eben) (computer . ?title)))
            (job (Scrooge Eben) (accounting chief accountant)))

       (and (supervisor (Aull DeWitt) (Warbucks Oliver))
            (not (job (Warbucks Oliver) (computer . ?title)))
            (job (Warbucks Oliver) (administration big wheel))))))

(deftest cydefect-replacements
  (returns-results
    '(replaces ?person (Fect Cy D))
    '((replaces (Hacker Alyssa P) (Fect Cy D))
      (replaces (Bitdiddle Ben) (Fect Cy D)))))

(deftest cheaper-replacements
  (returns-results
    '(and
       (replaces ?replacer ?replaced)
       (salary ?replacer ?replacer-salary)
       (salary ?replaced ?replaced-salary)
       (clojure-value < ?replacer-salary ?replaced-salary))
    '((and
        (replaces (Fect Cy D) (Hacker Alyssa P))
        (salary (Fect Cy D) 35000)
        (salary (Hacker Alyssa P) 40000)
        (clojure-value < 35000 40000))

      (and
        (replaces (Aull DeWitt) (Warbucks Oliver))
        (salary (Aull DeWitt) 25000)
        (salary (Warbucks Oliver) 150000)
        (clojure-value < 25000 150000)))))

(deftest big-shot
  (returns-results
    '(big-shot ?person accounting)
    '((big-shot (Scrooge Eben) accounting))))

(deftest meetings-by-day
  (returns-results
    '(meeting ?division (Friday ?time))
    '((meeting administration (Friday 13)))))

(deftest alyssa-wednesday-meetings
  (returns-results
    '(and (meeting-time (Hacker Alyssa P) (Wednesday ?time))
          (meeting ?division (Wednesday ?time)))
    '((and (meeting-time (Hacker Alyssa P) (Wednesday 15)) (meeting computer (Wednesday 15)))
      (and (meeting-time (Hacker Alyssa P) (Wednesday 13)) (meeting whole-company (Wednesday 13))))))

(deftest people-who-live-near
  (returns-results
    '(and (lives-near ?person-1 ?person-2)
          (clojure-value sicp.chapter-4.logic.clojure-value/name-before ?person-1 ?person-2))

    '((and (lives-near (Reasoner Louis) (Bitdiddle Ben)) (clojure-value sicp.chapter-4.logic.clojure-value/name-before (Reasoner Louis) (Bitdiddle Ben)))
      (and (lives-near (Reasoner Louis) (Aull DeWitt)) (clojure-value sicp.chapter-4.logic.clojure-value/name-before (Reasoner Louis) (Aull DeWitt)))
      (and (lives-near (Bitdiddle Ben) (Aull DeWitt)) (clojure-value sicp.chapter-4.logic.clojure-value/name-before (Bitdiddle Ben) (Aull DeWitt)))
      (and (lives-near (Hacker Alyssa P) (Fect Cy D)) (clojure-value sicp.chapter-4.logic.clojure-value/name-before (Hacker Alyssa P) (Fect Cy D))))))

(deftest next-to-1
  (returns-results
    sequence-operations
    '(?x next-to 1 in (2 1 3 1))

    '((2 next-to 1 in (2 1 3 1))
      (3 next-to 1 in (2 1 3 1)))))

(deftest next-to-2
  (returns-results
    sequence-operations
    '(?x next-to ?y in (1 (2 3) 4))

    '((1 next-to (2 3) in (1 (2 3) 4))
      ((2 3) next-to 4 in (1 (2 3) 4)))))

(deftest can-solve-base-case-of-last
  (returns-results
    sequence-operations
    '(last (3) ?x)
    '((last (3) (3)))))

(deftest can-reduce-to-last
  (returns-results
    sequence-operations
    '(last (1 2 3) ?x)
    '((last (1 2 3) (3)))))

(deftest can-insert-last-element
  (returns-results
    sequence-operations
    '(last (2 ?x) (3))
    '((last (2 3) (3)))))

(deftest grandsons-of-cain
  (returns-results
    bible-characters
    '(grandson Cain ?x)
    '((grandson Cain Irad))))

(deftest sons-of-lamech
  (returns-results
    bible-characters
    '(son Lamech ?x)
    '((son Lamech Jabal)
      (son Lamech Jubal))))

(deftest grandsons-of-Methushael
  (returns-results
    bible-characters
    '(grandson Methushael ?x)
    '((grandson Methushael Jabal)
      (grandson Methushael Jubal))))

(deftest great-grandsons
  (returns-results
    bible-characters
    '((great grandson) ?ggf ?ggs)
    '(((great grandson) Adam Irad)
      ((great grandson) Cain Mehujael)
      ((great grandson) Enoch Methushael)
      ((great grandson) Irad Lamech)
      ((great grandson) Mehujael Jabal)
      ((great grandson) Mehujael Jubal))))

(deftest Adam-Irad-relationship
  (returns-results
    bible-characters
    '(?relationship Adam Irad)
    '(((great grandson) Adam Irad))))

(deftest Adam-Mehujael-relationship
  (returns-results
    bible-characters
    '(?relationship Adam Mehujael)
    '(((great great grandson) Adam Mehujael))))

(deftest Cain-Methushael-relationship
  (returns-results
    bible-characters
    '(?relationship Cain Methushael)
    '(((great great grandson) Cain Methushael))))

(deftest Adam-Methushael-relationship
  (returns-results
    bible-characters
    '(?relationship Adam Methushael)
    '(((great great great grandson) Adam Methushael))))

(deftest Adam-Jubal-relationship
  (returns-results
    bible-characters
    '(?relationship Adam Jubal)
    '(((great great great great great grandson) Adam Jubal))))

(deftest Adam-gggggsons
  (returns-results
    bible-characters
    '((great great great great great grandson) Adam ?gggggson)
    '(((great great great great great grandson) Adam Jubal)
      ((great great great great great grandson) Adam Jabal))))

(deftest concatenate
  (returns-results
    sequence-operations
    '(append-to-form (a b) (c d) ?z)
    '((append-to-form (a b) (c d) (a b c d)))))

(deftest find-right-append
  (returns-results
    sequence-operations
    '(append-to-form (a b) ?y (a b c d))
    '((append-to-form (a b) (c d) (a b c d)))))

(deftest find-left-append
  (returns-results
    sequence-operations
    '(append-to-form ?x (c d) (a b c d))
    '((append-to-form (a b) (c d) (a b c d)))))

(deftest all-concatenating-lists
  (returns-results
    sequence-operations
    '(append-to-form ?x ?y (a b c d))
    '((append-to-form () (a b c d) (a b c d))
      (append-to-form (a) (b c d) (a b c d))
      (append-to-form (a b) (c d) (a b c d))
      (append-to-form (a b c) (d) (a b c d))
      (append-to-form (a b c d) () (a b c d)))))

(deftest simple-overflow
  (returns-results
    '((rule (married ?x ?y)
            (married ?y ?x))
      (married mickey minnie))
    '(married minnie ?x)
    '((married minnie mickey))))


(deftest reverse-right
  (returns-results
    sequence-operations
    '(reverse (3 2 1) ?x)
    '((reverse (3 2 1) (1 2 3)))))

(deftest reverse-left
  (returns-results
    sequence-operations
    '(reverse ?x (1 2 3))
    '((reverse (3 2 1) (1 2 3)))))


