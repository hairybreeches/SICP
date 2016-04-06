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

