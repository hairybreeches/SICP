(ns sicp.chapter-2.operations)


(defmulti get-record (fn [records id] (records :record-lookup)))
(defmulti get-salary (fn [record] (record :field-lookup)))

;map structure
(defmethod get-record :hashmap [records id] (records id))
(defmethod get-salary :hashmap [record] (record :salary))

;list structure
(defmethod get-record :list [records id] (first (filter #(= (first (% :record)) id) (records :records))))
(defmethod get-salary :list [record] (nth (:record record) 2))

(defn find-employee[id files]
  (first (filter #(not (nil? %)) (map #(get-record % id) files))))
