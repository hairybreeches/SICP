(ns sicp.test.chapter-4.logic
  (:use sicp.test.chapter-4.sample-database)
  (:use sicp.chapter-4.logic.querier)
  (:use sicp.test.assertions)
  (:use clojure.test))

(defn- returns-results
  [query expected-results]
  (is-set= (execute-query people query)
           (apply hash-set expected-results)))

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






