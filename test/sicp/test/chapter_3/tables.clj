(ns sicp.test.chapter-3.tables
  (:use sicp.chapter-3.tables)
  (:use clojure.test)
  (:use clojure.math.numeric-tower))

(deftest can-add-retrieve-keys
  (let [table (make-table)]
    ((table :insert!) :a 1)
    ((table :insert!) :b 2)
    ((table :insert!) :c 3)
    ((table :insert!) :d 4)
    ((table :insert!) :e 5)
    (is (= ((table :lookup) :a) 1))
    (is (= ((table :lookup) :b) 2))
    (is (= ((table :lookup) :c) 3))
    (is (= ((table :lookup) :d) 4))
    (is (= ((table :lookup) :e) 5))
    (is (= ((table :lookup) :f) false))))

(defn within-half?
  [a b]
  (< (abs (- a b)) 0.5))

(deftest can-retrieve-proximate-keys
  (let [table (make-table within-half?)]
    ((table :insert!) 1 :a)
    ((table :insert!) 2 :b)
    ((table :insert!) 3 :c)
    ((table :insert!) 4 :d)
    ((table :insert!) 5 :e)
    (is (= ((table :lookup) 1.2) :a))
    (is (= ((table :lookup) 2.4) :b))
    (is (= ((table :lookup) 2.8) :c))
    (is (= ((table :lookup) 3.6) :d))
    (is (= ((table :lookup) 5.4) :e))
    (is (= ((table :lookup) 5.6) false))))
