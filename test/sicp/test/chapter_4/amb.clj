(ns sicp.test.chapter-4.amb
  (:use sicp.chapter-4.interpreter.repl)
  (:use clojure.test))

(deftest amb-returns-results
  (is
    (=
      (get-all-results '(amb 1 2 3))
      '(1 2 3))))

(def require-code
  '(define (require p)
             (if (not p) (amb))))

(def an-integer-between
  '(define (an-integer-between low high)
             (if (> low high)
               (amb)
               (amb low (an-integer-between (+ 1 low) high)))))

(def an-integer-starting-from
  '(define (an-integer-starting-from low)
     (amb low (an-integer-starting-from (+ 1 low)))))

(def filter-code
  '(define (filter predicate things)
     (cond ((null? things) '())
           ((predicate (car things)) (cons (car things) (filter predicate (cdr things))))
           (else (filter predicate (cdr things))))))

(def map-code
  '(define (map transformer things)
     (if
       (null? things)
       '()
       (cons (transformer (car things)) (map transformer (cdr things))))))

(def an-element-of
  '(define (an-element-of things)
     (require (not (null? things)))
     (amb (car things) (an-element-of (cdr things)))))

(def member?
  '(define (member? collection object)
     (cond ((null? collection) false)
           ((= (car collection) object) true)
           (else (member? (cdr collection) object)))))

(def exclude-code
  '(define (exclude excluded objects)
     (filter
       (lambda
         (object)
         (not (member? excluded object)))
       objects)))

(deftest pythagorean-triples
  (is
    (=
      (get-all-results
           require-code
           an-integer-between

          '(define (a-pythagorean-triple-between low high)
             (let ((high-squared (* high high)))
               (let ((i (an-integer-between low high)))
                 (let ((i-squared (* i i)))
                   (let ((j-max (sqrt (- high-squared i-squared))))
                     (let ((j (an-integer-between i j-max)))
                       (let ((k (sqrt (+ i-squared (* j j)))))
                         (require (integer? k))
                         (list i j k))))))))

          '(a-pythagorean-triple-between 1 13)
           )

      '((3 4 5) (5 12 13) (6 8 10)))))

(deftest infinite-pythagorean-triples
  (is
    (=
      (take 4
         (get-all-results
            require-code
            an-integer-between
            an-integer-starting-from

           '(define (a-pythagorean-triple)
             (let ((root2 (sqrt 2)))
               (let ((hypotenuse (an-integer-starting-from 5)))
                 (let ((hypotenuse-squared (* hypotenuse hypotenuse))
                       (j-max (- hypotenuse 1))
                       (j-min (+ 1 (int (/ hypotenuse root2)))))
                     (let ((j (an-integer-between j-min j-max)))
                       (let ((i (sqrt (- hypotenuse-squared (* j j)))))
                         (require (integer? i))
                         (list i j hypotenuse)))))))

           '(a-pythagorean-triple)
           ))

      '((3 4 5) (6 8 10) (5 12 13) (9 12 15)))))

(deftest multiple-dwelling-amb
  (is (=
        (get-all-results
          require-code
          an-element-of
          member?
          filter-code
          exclude-code

          '(define (baker-options)
             '(1 2 3 4))

          '(define (cooper-options baker)
             (exclude (list baker) '(2 3 4 5)))

          '(define (fletcher-options baker cooper)
             (exclude (list baker cooper (+ cooper 1) (- cooper 1)) '(2 3 4)))

          '(define (miller-options baker cooper fletcher)
             (filter (lambda (option) (> option cooper)) (exclude (list baker cooper fletcher) '(1 2 3 4 5))))

          '(define (smith-options baker cooper fletcher miller)
             (exclude (list baker cooper fletcher miller (+ fletcher 1) (- fletcher 1)) '(1 2 3 4 5)))

          '(let ((baker (an-element-of (baker-options))))
             (let ((cooper (an-element-of (cooper-options baker))))
               (let ((fletcher (an-element-of (fletcher-options baker cooper))))
                 (let ((miller (an-element-of (miller-options baker cooper fletcher))))
                   (let ((smith (an-element-of (smith-options baker cooper fletcher miller))))
                     (list (list 'baker baker)
                           (list 'cooper cooper)
                           (list 'fletcher fletcher)
                           (list 'miller miller)
                           (list 'smith smith))))))))

        '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))
        )))

(deftest liars
  (is (=
        (get-all-results
          require-code
          an-element-of
          '(let ((betty (amb 1 2 3 4 5))
                 (ethel (amb 1 2 3 4 5))
                 (joan (amb 1 2 3 4 5))
                 (kitty (amb 1 2 3 4 5))
                 (mary (amb 1 2 3 4 5)))
             (require (distinct? betty ethel joan kitty mary))
             (require (xor (= kitty 2) (= betty 3)))
             (require (xor (= ethel 1) (= joan 2)))
             (require (xor (= joan 3) (= ethel 5)))
             (require (xor (= kitty 2) (= mary 4)))
             (require (xor (= mary 4) (= betty 1)))
             (list (list 'betty betty)
                   (list 'ethel ethel)
                   (list 'joan joan)
                   (list 'kitty kitty)
                   (list 'mary mary))))
        '(((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))))))

(deftest yachts
  (is (= (get-all-results
           require-code
           an-element-of
           filter-code
           exclude-code
           member?

           '(let ((melissa 'hood))
              (let ((mary (an-element-of (exclude (list melissa) '(downing hall hood moore)))))
              (let ((lorna (an-element-of (exclude (list melissa mary) '(downing hall parker hood)))))
                (let ((rosalind (an-element-of (exclude (list lorna melissa mary) '(downing parker hood moore)))))
                  (let ((gabrielle (an-element-of (exclude (list lorna melissa mary rosalind) '(downing hall moore)))))
                    (let ((gabrielles-dads-yachts-father
                          (cond ((= gabrielle 'downing) melissa)
                                ((= gabrielle 'hall) rosalind)
                                ((= gabrielle 'moore) lorna))))
                  (require (= gabrielles-dads-yachts-father 'parker))
                        (list
                          'melissa melissa
                          'mary mary
                          'lorna lorna
                          'rosalind rosalind
                          'gabrielle gabrielle))))))))

         '((melissa hood mary hall lorna parker rosalind downing gabrielle moore)
           (melissa hood mary moore lorna downing rosalind parker gabrielle hall)))))

(deftest queens
  (is (= (take 10 (get-all-results
           require-code
           an-element-of
           filter-code
           exclude-code
           member?
           map-code

           '(define (make-position column row)
              (list column row))

           '(define (get-column position)
              (car position))

           '(define (get-row position)
              (car (cdr position)))

           '(define (get-positions column)
              (map (lambda (row) (make-position column row)) '(1 2 3 4 5 6 7 8)))

           '(define (clash? pos1 pos2)
              (or (= (get-row pos1) (get-row pos2))
                  (= (get-column pos1) (get-column pos2))
                  (= (abs (- (get-column pos1) (get-column pos2)))
                     (abs (- (get-row pos1) (get-row pos2))))))

           '(define (safe? position existing)
              (null?
                (filter
                  (lambda (pos2) (clash? position pos2))
                  existing)))


           '(define (get-safe-positions column existing)
              (filter
                (lambda (position) (safe? position existing))
                (get-positions column)))


           '(define (queens column)
              (if (= column 0)
                  '()
                  (let ((existing-solution (queens (- column 1))))
                    (cons (an-element-of (get-safe-positions column existing-solution))
                          existing-solution))))

           '(queens 8)))
         '(((8 4) (7 2) (6 7) (5 3) (4 6) (3 8) (2 5) (1 1))
           ((8 5) (7 2) (6 4) (5 7) (4 3) (3 8) (2 6) (1 1))
           ((8 3) (7 5) (6 2) (5 8) (4 6) (3 4) (2 7) (1 1))
           ((8 3) (7 6) (6 4) (5 2) (4 8) (3 5) (2 7) (1 1))
           ((8 5) (7 7) (6 1) (5 3) (4 8) (3 6) (2 4) (1 2))
           ((8 4) (7 6) (6 8) (5 3) (4 1) (3 7) (2 5) (1 2))
           ((8 3) (7 6) (6 8) (5 1) (4 4) (3 7) (2 5) (1 2))
           ((8 5) (7 3) (6 8) (5 4) (4 7) (3 1) (2 6) (1 2))
           ((8 5) (7 7) (6 4) (5 1) (4 3) (3 8) (2 6) (1 2))
           ((8 4) (7 1) (6 5) (5 8) (4 6) (3 3) (2 7) (1 2))))))












