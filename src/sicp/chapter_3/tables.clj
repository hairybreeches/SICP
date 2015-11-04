(ns sicp.chapter-3.tables
  (:use sicp.chapter-3.lists)
  (:use sicp.chapter-2.pairs))


(defn- find-record
  [kee records same-key?]
  (loop [records records]
    (cond (nil? records) false
          (same-key? kee (car (car records))) (car records)
          :else (recur (cdr records)))))

(defn make-table
  ([same-key?]
  (let
    [local-table (cons-pair :table nil)]

    (defn find-local-record
      [kee]
      (find-record kee (cdr local-table) same-key?))

    (defn lookup
      [kee]
      (let [record (find-local-record kee)]
        (if record
            (cdr record)
            false)))

    (defn insert!
      [kee value]
        (let [record (find-local-record kee)]
            (if record
                (set-cdr! record value)
                (set-cdr! local-table (cons-pair (cons-pair kee value) (cdr local-table))))))

    (defn dispatch
      [m]
      (cond (= m :lookup) lookup
            (= m :insert!) insert!))))
     ([]
      (make-table =)))

