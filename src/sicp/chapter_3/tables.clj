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
      [local-table (cons-pair :table nil)

       find-local-record
       (fn
         [kee]
         (find-record kee (cdr local-table) same-key?))

       lookup
       (fn
          [kee]
          (let [record (find-local-record kee)]
            (if record
                (cdr record)
                false)))

       insert!
       (fn [value kee]
           (let [record (find-local-record kee)]
                (if record
                    (set-cdr! record value)
                    (set-cdr! local-table (cons-pair (cons-pair kee value) (cdr local-table))))))

       dispatch
       (fn [m]
           (cond (= m :lookup) lookup
                 (= m :insert!) insert!))]

    dispatch))

     ([]
      (make-table =)))


(defn make-multi-table
  [same-key? depth]
  (if (= depth 1)
      (make-table same-key?)
      (let [table (make-table same-key?)

            lookup-record
            (fn [kee]
              ((table :lookup) kee))

            inner-lookup
            (fn [record kees]
                (apply (record :lookup) kees))

            inner-insert!
            (fn [record value kees]
                (apply (record :insert!) value kees))

            create-record
            (fn [kee]
                (let [record (make-multi-table same-key? (dec depth))]
                      ((table :insert!) record kee)
                      record))

            get-or-create-record!
            (fn [kee]
                (let [current (lookup-record kee)]
                    (if current
                        current
                        (create-record kee))))

            lookup
            (fn [& kees]
                (let [record (lookup-record (first kees))]
                     (if record
                         (inner-lookup record (rest kees))
                        false)))

            insert!
            (fn [value & kees]
                (let [record (get-or-create-record! (first kees))]
                     (inner-insert! record value (rest kees))))

            dispatch
            (fn [m]
                (cond (= m :lookup) lookup
                      (= m :insert!) insert!))
            ]

        dispatch)))

