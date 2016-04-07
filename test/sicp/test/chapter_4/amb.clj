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

(def filter-code
  '(define (filter predicate things)
     (cond ((null? things) '())
           ((predicate (car things)) (cons (car things) (filter predicate (cdr things))))
           (else (filter predicate (cdr things))))))

(def an-element-of
  '(define (an-element-of things)
     (require (not (null? things)))
     (amb (car things) (an-element-of (cdr things)))))

(def member?
  '(define (member? collection object)
     (cond ((null? collection) false)
           ((= (car collection) object) true)
           (else (member? (cdr collection) object)))))

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

           '(define (an-integer-starting-from low)
              (amb low (an-integer-starting-from (+ 1 low))))

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

          '(define (exclude excluded objects)
             (filter
             (lambda
               (object)
               (not (member? excluded object)))
               objects))

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

(defn exclude
  [options & to-exclude]
  (let [exclude-set (apply hash-set to-exclude)]
   (filter
     #(not (exclude-set %))
     options)))

(defn baker-options []
  '(1 2 3 4))

(defn cooper-options [so-far]
  (let [baker (:baker so-far)]
    (exclude '(2 3 4 5) baker)))

(defn fletcher-options [so-far]
  (let [baker (:baker so-far)
        cooper (:cooper so-far)]
  (exclude '(2 3 4) baker cooper (+ cooper 1) (- cooper 1))))

(defn miller-options [so-far]
  (let [baker (:baker so-far)
        cooper (:cooper so-far)
        fletcher (:fletcher so-far)]
  (filter
    #(> % cooper)
    (exclude '(1 2 3 4 5) baker cooper fletcher))))

(defn smith-options [so-far]
  (let [baker (:baker so-far)
        cooper (:cooper so-far)
        fletcher (:fletcher so-far)
        miller (:miller so-far)]
  (exclude '(1 2 3 4 5) baker cooper fletcher miller (+ fletcher 1) (- fletcher 1))))

(defn add-options [so-far n options]
  (map #(assoc so-far n %) options))

(defn refine-options [generator n options-so-far]
  (mapcat #(add-options % n (generator %)) options-so-far))

(defn multiple-dwelling []
  (->>
    (baker-options)
    (map (fn [%] {:baker %}))
    (refine-options cooper-options :cooper)
    (refine-options fletcher-options :fletcher)
    (refine-options miller-options :miller)
    (refine-options smith-options :smith)))

(deftest multiple-dwelling-clojure
  (is (= (multiple-dwelling) '({:baker 3 :cooper 2 :fletcher 4 :miller 5 :smith 1}))))

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




