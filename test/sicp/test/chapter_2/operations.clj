(ns sicp.test.chapter-2.operations
  (:use sicp.chapter-2.operations)
  (:use clojure.test))

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

