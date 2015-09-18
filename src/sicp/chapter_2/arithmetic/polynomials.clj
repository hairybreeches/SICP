(ns sicp.chapter-2.arithmetic.polynomials
  (:use sicp.chapter-2.arithmetic.universal-arithmetic))

;termlist
(def empty-termlist?
  empty?)

(defn rest-terms [l]
  (with-meta
    (rest l)
    (meta l)))

(def first-term
  first)

(def last-term
  last)

;termlist interface
(defn get-format[object]
  (:format (meta object)))

(defn get-format-of-first[a & args]
  (get-format a))

(defn get-formats[a b]
  (let [aformat (get-format a)
        bformat (get-format b)]
    (if (= aformat bformat)
        aformat
        (throw (Exception. (str "mixed termlist types!:" aformat bformat))))))

(defmulti constant-term get-format)
(defmulti term-list= get-formats)
(defmulti add-terms get-formats)
(defmulti mul-term-by-all-terms get-format-of-first)

(defn mul-terms [l1 l2]
  (if (empty-termlist? l1) l1
      (add-terms (mul-term-by-all-terms l2 (first-term l1))
                 (mul-terms (rest-terms l1) l2))))

;sparse termlist representation
(defn make-term[order coeff]
  (list order coeff))

(defn coeff [term]
  (second term))

(defn order [term]
  (first term))


(defmethod constant-term :sparse [l] (coeff (last-term l)))

(defn term= [a b]
  (and (equ? (coeff a)
             (coeff b))
       (= (order a)
          (order b))))

(defmethod term-list= :sparse [a b]
  (cond (empty-termlist? a) (empty-termlist? b)
        (empty-termlist? b) false
        :else (and (term= (first-term a) (first-term b))
                   (term-list= (rest-terms a) (rest-terms b)))))

(defn make-sparse-termlist[& args]
  (with-meta
    (apply list args)
    {:format :sparse}))


(defn adjoin-term [term term-list]
  (with-meta
    (if (equ? 0 (coeff term))
      term-list
      (cons term term-list))
      (meta term-list)))

(defmethod add-terms :sparse [l1 l2]
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

(defmethod mul-term-by-all-terms :sparse [termlist t1]
  (if (empty-termlist? termlist)
      termlist
      (let [t2 (first-term termlist)]
          (adjoin-term
             (make-term (+ (order t1) (order t2))
                        (mul (coeff t1) (coeff t2)))
             (mul-term-by-all-terms (rest-terms termlist) t1)))))

;polynomials
(defn make-poly [var terms]
   ^{:type ::polynomial}
    {:variable var :terms terms})

(defn term-list [poly]
  (:terms poly))

(defn variable [poly]
  (:variable poly))

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

(defn equ?-poly[a b]
  (and (= (variable a) (variable b))
       (term-list= (term-list a) (term-list b))))

(defmethod add-pair ::polynomial [a b] (add-poly a b))
(defmethod mul-pair ::polynomial [a b] (mul-poly a b))
(defmethod equ? ::polynomial [a b] (equ?-poly a b))

;this isn't a great decision to just pick a variable, but I don't think it will matter yet.
(defmethod raise :sicp.chapter-2.arithmetic.complex-numbers/complex [a] (make-poly 'x (make-sparse-termlist (make-term 0 a))))
(defmethod number-project ::polynomial [a] (constant-term (term-list a)))
(derive :sicp.chapter-2.arithmetic.complex-numbers/complex ::polynomial)

