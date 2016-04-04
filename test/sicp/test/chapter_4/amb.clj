(ns sicp.test.chapter-4.amb
  (:use sicp.chapter-4.interpreter.repl)
  (:use clojure.test))

(deftest amb-returns-results
  (is
    (=
      (get-all-results '(amb 1 2 3))
      '(1 2 3))))

(deftest pythagorean-triples
  (is
    (=
      (get-all-results
        '(begin

           (define (require p)
             (if (not p) (amb)))

           (define (an-integer-between low high)
             (if (> low high)
               (amb)
               (amb low (an-integer-between (+ 1 low) high))))

           (define (a-pythagorean-triple-between low high)
             (let ((high-squared (* high high)))
               (let ((i (an-integer-between low high)))
                 (let ((i-squared (* i i)))
                   (let ((j-max (sqrt (- high-squared i-squared))))
                     (let ((j (an-integer-between i j-max)))
                       (let ((k (sqrt (+ i-squared (* j j)))))
                         (require (integer? k))
                         (list i j k))))))))

           (a-pythagorean-triple-between 1 13)
           ))

      '((3 4 5) (5 12 13) (6 8 10)))))

