(ns sicp.test.chapter-4.logic
  (:use sicp.chapter-4.logic)
  (:use clojure.test))

(def people
  '((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (job (Bitdiddle Ben) (computer wizard))
    (salary (Bitdiddle Ben) 60000)
    (supervisor (Bitdiddle Ben) (Warbucks Oliver))

    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job (Hacker Alyssa P) (computer programmer))
    (salary (Hacker Alyssa P) 40000)
    (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (job (Fect Cy D) (computer programmer))
    (salary (Fect Cy D) 35000)
    (supervisor (Fect Cy D) (Bitdiddle Ben))

    (address (Tweakit Lem E) (Boston (Bay State Road) 22))
    (job (Tweakit Lem E) (computer technician))
    (salary (Tweakit Lem E) 25000)
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))

    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (job (Reasoner Louis) (computer programmer trainee))
    (salary (Reasoner Louis) 30000)
    (supervisor (Reasoner Louis) (Hacker Alyssa P))

    (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (job (Warbucks Oliver) (administration big wheel))
    (salary (Warbucks Oliver) 150000)

    (address (Scrooge Eben) (Weston (Shady Lane) 10))
    (job (Scrooge Eben) (accounting chief accountant))
    (salary (Scrooge Eben) 75000)
    (supervisor (Scrooge Eben) (Warbucks Oliver))

    (address (Cratchet Robert) (Allston (N Harvard Street) 16))
    (job (Cratchet Robert) (accounting scrivener))
    (salary (Cratchet Robert) 18000)
    (supervisor (Cratchet Robert) (Scrooge Eben))

    (address (Aull DeWitt) (Slumerville (Onion Square) 5))
    (job (Aull DeWitt) (administration secretary))
    (salary (Aull DeWitt) 25000)
    (supervisor (Aull DeWitt) (Warbucks Oliver))

    (can-do-job (computer wizard) (computer programmer))
    (can-do-job (computer wizard) (computer technician))
    (can-do-job (computer programmer) (computer programmer trainee))
    (can-do-job (administration secretary) (administration big wheel))))


(deftest retrieve-by-supervisor
  (is (=
        (execute-query
          people
          '(supervisor ?x (Bitdiddle Ben)))

        '((supervisor (Hacker Alyssa P) (Bitdiddle Ben))
          (supervisor (Fect Cy D) (Bitdiddle Ben))
          (supervisor (Tweakit Lem E) (Bitdiddle Ben))))))

(deftest retrieve-by-division
  (is (=
        (execute-query
          people
          '(job ?name (accounting . ?job)))

        '((job (Scrooge Eben) (accounting chief accountant))
          (job (Cratchet Robert) (accounting scrivener))))))

(deftest retrieve-by-town
  (is (=
        (execute-query
          people
          '(address ?name (Slumerville . ?address)))

        '((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
          (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
          (address (Aull DeWitt) (Slumerville (Onion Square) 5))))))



