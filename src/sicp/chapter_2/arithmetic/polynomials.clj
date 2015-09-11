(ns sicp.chapter-2.arithmetic.polynomials
  (:use sicp.chapter-2.arithmetic.universal-arithmetic))

(defn make-term[order coeff]
  (list order coeff))

(defn coeff [term]
  (second term))

(defn order [term]
  (first term))

(def empty-termlist?
  empty?)

(def rest-terms
  rest)

(def first-term
  first)

(defn the-empty-termlist []
  '())

(defn make-termlist[& args]
  (apply list args))


(defn adjoin-term [term term-list]
  (if (equ? 0 (coeff term))
      term-list
      (cons term term-list)))

(defn add-terms [l1 l2]
  (cond (empty-termlist? l1) l2
        (empty-termlist? l2) l1
        :else (let [t1 (first-term l1)
                    t2 (first-term l2)]
                (cond (> (order t1) (order t2))
                        (adjoin-term t1 (add-terms (rest-terms l1) l2))
                      (> (order t2) (order t1))
                        (adjoin-term t2 (add-terms (rest-terms l2) l1))
                      :else
                        (adjoin-term (make-term (order t1)
                                                (add (coeff t1) (coeff t2)))
                                     (add-terms (rest-terms l1)
                                                (rest-terms l2)))))))

(defn mul-term-by-all-terms [t1 termlist]
  (if (empty-termlist? termlist)
      (the-empty-termlist)
      (let [t2 (first-term termlist)]
          (adjoin-term
             (make-term (+ (order t1) (order t2))
                        (mul (coeff t1) (coeff t2)))
             (mul-term-by-all-terms t1 (rest-terms termlist))))))

(defn make-poly [var terms]
   ^{:type ::polynomial}
    {:variable var :terms terms})

(defn term-list [poly]
  (:terms poly))

(defn variable [poly]
  (:variable poly))



(defn mul-terms [l1 l2]
  (if (empty-termlist? l1) (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term l1) l2)
                 (mul-terms (rest-terms l1) l2))))



(defn add-poly [p1 p2]
  (if (= (variable p1) (variable p2))
    (make-poly (variable p1)
               (add-terms (term-list p1)
                          (term-list p2)))
    (throw (Exception. (str "polys not same variable: " (variable p1) (variable p2))))))

(defn mul-poly [p1 p2]
  (if (= (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (throw (Exception. (str "polys not same variable: " (variable p1) (variable p2))))))

