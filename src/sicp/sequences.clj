(ns sicp.sequences
  (:use sicp.error))

(defn non-empty-seq? [e]
  (and (seq? e)
       (not (empty? e))))

(defn- process-dots
  [r]
  (cond (not (= (first r) '.)) r
        (= (count r) 2) (second r)
        :else (error "cannot use dot notation with more than one subsequent: " r)))


(defn scheme-rest [s]
  (process-dots (rest s)))

;copied from http://www.futurerust.com/2014/05/18/interleave-all-the-things/
(defn interleave-all
  "Returns a lazy seq of the first item in each coll, then the second etc.
   for all values in each coll"
  ([c] c)
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (cond (and s1 s2)
          (cons (first s1) (cons (first s2)
                                 (interleave-all (rest s1) (rest s2))))
          :else (concat s1 s2)))))
  ([c1 c2 & colls]
     (lazy-seq
      (let [ss (filter identity (map seq (conj colls c2 c1)))]
        (concat (map first ss) (apply interleave-all (map rest ss)))))))

