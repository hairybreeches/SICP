(ns sicp.chapter-1.ex-31
  (:use sicp.chapter-1.ex-32))

(defn product [term start iterator finish]
  (accumulate * 1 term start iterator finish))


(defn factorial [n]
  (product identity 1 inc n))

(defn round-up-to-nearest-even[n]
  (if
    (even? n)
    n
    (inc n)))

(defn round-up-to-nearest-odd[n]
  (if
    (even? n)
    (inc n)
    n))

(defn pi-term-denominator [n]
  (round-up-to-nearest-odd (inc n)))

(defn pi-term-numerator [n]
  (round-up-to-nearest-even (inc n)))


(defn pi-term [n]
  (float (/ (pi-term-numerator n) (pi-term-denominator n))))


(defn pi [number-of-terms]
  (* 4 (product pi-term 1 inc number-of-terms)))


