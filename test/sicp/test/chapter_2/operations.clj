(ns sicp.test.chapter-2.operations
  (:use sicp.chapter-2.operations)
  (:use clojure.test)
  (:use clojure.math.numeric-tower)
  (:use sicp.test.assertions))

(def map-records-file {:record-lookup :hashmap "employee #12" {:name "steve" :salary 40000 :field-lookup :hashmap} })

(def list-records-file {:record-lookup :list :records '({:field-lookup :list :record ("employee #11" "bert" 60000)})})

(def files [map-records-file list-records-file])

(deftest can-get-record-from-map-file
  (is (= (get-record map-records-file "employee #12") {:name "steve" :salary 40000 :field-lookup :hashmap})))

(deftest can-get-record-from-list-file
  (is (= (get-record list-records-file "employee #11") {:field-lookup :list :record '("employee #11" "bert" 60000)})))

(deftest can-get-salary-from-map-file
  (is (= (get-salary (get-record map-records-file "employee #12")) 40000)))

(deftest can-get-salary-from-list-file
  (is (= (get-salary (get-record list-records-file "employee #11")) 60000)))

(deftest can-search-files-for-list-employee
  (is (= (get-salary (find-employee "employee #11" files)) 60000)))

(deftest can-search-files-for-map-employee
  (is (= (get-salary (find-employee "employee #12" files)) 40000)))

(def pi java.lang.Math/PI)

(deftest can-get-real-from-real-imag-number
  (is (= (apply-generic 'real-part (make-from-real-imag 3 4)) 3)))

(deftest can-get-imag-from-real-imag-number
  (is (= (apply-generic 'imag-part (make-from-real-imag 3 4)) 4)))

(deftest can-get-magnitude-from-real-imag-number
  (is (= (apply-generic 'magnitude (make-from-real-imag 3 4)) 5)))

(deftest can-get-angle-from-real-imag-number
  (is (= (apply-generic 'angle (make-from-real-imag 5 5)) (/ pi 4))))

(deftest can-get-real-from-polar-number
  (is (roughly= (apply-generic 'real-part (make-from-polar 2 (/ pi 3))) 1 8)))

(deftest can-get-imag-from-polar-number
  (is (= (apply-generic 'imag-part (make-from-polar 2 (/ pi 3))) (sqrt 3))))

(deftest can-get-magnitude-from-polar-number
  (is (= (apply-generic 'magnitude (make-from-polar 2 (/ pi 3))) 2)))

(deftest can-get-angle-from-polar-number
  (is (= (apply-generic 'angle (make-from-polar 3 (/ pi 3))) (/ pi 3))))

