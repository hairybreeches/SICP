(ns sicp.chapter-4.logic.sample-database)

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
    (can-do-job (administration secretary) (administration big wheel))

    (rule (same ?x ?x))

    (rule (replaces ?person1 ?person2)
          (and (job ?person1 ?person1-job)
               (job ?person2 ?person2-job)
               (or (same ?person1-job ?person2-job)
                   (can-do-job ?person1-job ?person2-job))
               (not (same ?person1 ?person2))))

    (rule (big-shot ?person ?division)
          (and (job ?person (?division . ?role))
               (not (and (supervisor ?person ?supervisor)
                         (job ?supervisor (?division . ?supervisor-role))))))

    (meeting accounting (Monday 9))
    (meeting administration (Monday 10))
    (meeting computer (Wednesday 15))
    (meeting administration (Friday 13))
    (meeting whole-company (Wednesday 13))

    (rule (meeting-time ?person ?day-and-time)
          (or
            (meeting whole-company ?day-and-time)
            (and
              (job ?person (?division . ?title))
              (meeting ?division ?day-and-time))))

    (rule (lives-near ?person1 ?person2)
          (and (address ?person1 (?town . ?rest1))
               (address ?person2 (?town . ?rest2))
               (not (same ?person1 ?person2))))

    ))

(def sequence-operations
  '((rule (?x next-to ?y in (?x ?y . ?u)))
    (rule (?x next-to ?y in (?v . ?z))
          (?x next-to ?y in ?z))

    (rule (last (?x) (?x)))
    (rule (last (?y ?z . ?u) (?x))
          (last (?z . ?u) (?x)))

    (rule (append-to-form () ?y ?y))
    (rule (append-to-form (?u . ?v) ?y (?u . ?z))
          (append-to-form ?v ?y ?z))

    (reverse () ())
    (rule (reverse ?x ?y)
          (reverse ?y ?x))
    (rule (reverse (?x . ?y) ?z)
          (and
            (reverse ?y ?rev-y)
            (append-to-form ?rev-y (?x) ?z)))))

(def bible-characters
  (concat
    sequence-operations
    '((son Adam Cain)
      (son Cain Enoch)
      (son Enoch Irad)
      (son Irad Mehujael)
      (son Mehujael Methushael)
      (son Methushael Lamech)
      (wife Lamech Ada)
      (son Ada Jabal)
      (son Ada Jubal)
      (rule (son ?father ?son)
            (and (wife ?father ?mother)
                 (son ?mother ?son)))
      (rule (grandson ?grandfather ?grandson)
            (and (son ?grandfather ?father)
                 (son ?father ?grandson)))

      (rule ((great grandson) ?great-grandfather ?great-grandson)
            (and (son ?great-grandfather ?grandfather)
                 (grandson ?grandfather ?great-grandson)))

      (rule ((great . ?x) ?ancestor ?descendant)
            (and
              (son ?ancestor ?intermediate)
              (?x ?intermediate ?descendant)
              (last ?x (grandson))))

      )))
