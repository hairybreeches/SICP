(ns sicp.chapter-4.amb-code
  (:use sicp.chapter-4.sequences)
  (:use sicp.chapter-4.interpreter.repl))

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

(def an-element-of
  '(define (an-element-of things)
     (require (not (null? things)))
     (amb (car things) (an-element-of (cdr things)))))

(def a-pythagorean-triple-between
  '(define (a-pythagorean-triple-between low high)
     (let ((high-squared (* high high)))
       (let ((i (an-integer-between low high)))
         (let ((i-squared (* i i)))
           (let ((j-max (sqrt (- high-squared i-squared))))
             (let ((j (an-integer-between i j-max)))
               (let ((k (sqrt (+ i-squared (* j j)))))
                 (require (integer? k))
                 (list i j k)))))))))

(def a-pythagorean-triple
  '(define (a-pythagorean-triple)
     (let ((root2 (sqrt 2)))
       (let ((hypotenuse (an-integer-starting-from 5)))
         (let ((hypotenuse-squared (* hypotenuse hypotenuse))
               (j-max (- hypotenuse 1))
               (j-min (+ 1 (int (/ hypotenuse root2)))))
           (let ((j (an-integer-between j-min j-max)))
             (let ((i (sqrt (- hypotenuse-squared (* j j)))))
               (require (integer? i))
               (list i j hypotenuse))))))))

(defn get-pythagorean-triples []
  (get-all-results
    require-code
    an-integer-between
    an-integer-starting-from
    a-pythagorean-triple
    '(a-pythagorean-triple)))

(defn get-pythagorean-triples-between [low high]
  (get-all-results
    require-code
    an-integer-between
    a-pythagorean-triple-between
    (list 'a-pythagorean-triple-between low high)))

(defn get-multiple-dwelling-solutions []
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
                     (list 'smith smith)))))))))

(defn get-liars-solutions []
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
             (list 'mary mary)))))

(defn get-queens-solutions []
  (get-all-results
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

(defn get-yachts-solutions []
  (get-all-results
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
                   'gabrielle gabrielle)))))))))
