(ns sicp.chapter-2.arithmetic.elements.polynomials
  (:use clojure.math.numeric-tower)
  (:use sicp.chapter-2.arithmetic.arithmetic-operations)
  (:use sicp.chapter-2.arithmetic.numerical-type-system))

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
(defn get-format-of-first[a & args]
  (get-format a))

(defn get-formats[a b]
  (let [aformat (get-format a)
        bformat (get-format b)]
    (if (= aformat bformat)
        aformat
        :mixed)))

(defmulti constant-term get-format)
(defmulti term-list= get-formats)
(defmulti add-terms get-formats)
(defmulti term-list-order get-format)
(defmulti get-leading-coefficient get-format)
(defmulti mul-term-by-all-terms get-format-of-first)
(defmulti to-sparse-format get-format)


(defn mul-terms [l1 l2]
  (if (empty-termlist? l1) l1
      (add-terms (mul-term-by-all-terms l2 (term-list-order l1) (get-leading-coefficient l1))
                 (mul-terms (rest-terms l1) l2))))

;sparse termlist representation
(defn make-term[order coeff]
  (list order coeff))

(defn coeff [term]
  (second term))

(defn order [term]
  (first term))

(defmethod term-list-order :sparse [l]
  (order (first-term l)))

(defmethod get-leading-coefficient :sparse [l]
  (coeff (first-term l)))

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

(defmethod mul-term-by-all-terms :sparse [termlist term-order term-coefficient]
  (if (empty-termlist? termlist)
      termlist
      (let [t2 (first-term termlist)]
          (adjoin-term
             (make-term (+ term-order (order t2))
                        (mul term-coefficient (coeff t2)))
             (mul-term-by-all-terms (rest-terms termlist) term-order term-coefficient)))))

(defmethod to-sparse-format :sparse [termlist]
  termlist)

;dense termlist representation

(defn make-dense-termlist[& terms]
    (with-meta
      (apply list (drop-while #(= % 0) terms))
      {:format :dense}))

(defmethod term-list-order :dense [termlist]
  (dec (count termlist)))

(defmethod get-leading-coefficient :dense [termlist]
  (first termlist))

(defmethod constant-term :dense [l] (last-term l))

(defmethod term-list= :dense [l m] (= l m))

(defn add-equal-length-term-lists [l m]
  (apply make-dense-termlist (reverse (map add (reverse l)  (reverse m)))))

(defn pad-terms[l m]
  (let [length-l (count l)
        length-m (count m)
        difference (abs (- length-l length-m))
        padding (repeat difference 0)]
    (if (> length-l length-m)
        [l (concat padding m)]
        [(concat padding l) m])))


(defmethod add-terms :dense [l m]
  (apply add-equal-length-term-lists (pad-terms l m)))


(defmethod mul-term-by-all-terms :dense [termlist term-order term-coefficient]
  (apply make-dense-termlist (concat (map #(mul % term-coefficient) termlist) (repeat term-order 0))))

(defmethod to-sparse-format :dense [termlist]
  (apply make-sparse-termlist (map make-term (reverse (range 0 (inc (term-list-order termlist)))) termlist )))

;mixed operations
(defmethod add-terms :mixed [& args]
  (apply add-terms (map to-sparse-format args)))

(defmethod term-list= :mixed [& args]
  (apply term-list= (map to-sparse-format args)))

;polynomials
(defn make-poly [var terms]
   ^{:type ::polynomial}
    {:variable var :terms terms})

(defn term-list [poly]
  (:terms poly))

(defn variable [poly]
  (:variable poly))

(defn polynomial-order[poly]
  (term-list-order (term-list poly)))

(defn shared-variable [p1 p2]
  (cond (= (variable p1) (variable p2)) (variable p1)
        (= (polynomial-order p2) 0) (variable p1)
        (= (polynomial-order p1) 0) (variable p2)
        :else (throw (Exception. (str "polys not same variable: " (variable p1) (variable p2))))))

(defn add-poly [p1 p2]
  (make-poly (shared-variable p1 p2)
             (add-terms (term-list p1)
                        (term-list p2))))

(defn mul-poly [p1 p2]
  (make-poly (shared-variable p1 p2)
             (mul-terms (term-list p1)
                        (term-list p2))))

(defn equ?-poly[a b]
  (and (= (variable a) (variable b))
       (term-list= (term-list a) (term-list b))))

(defmethod add-pair ::polynomial [a b] (add-poly a b))
(defmethod mul-pair ::polynomial [a b] (mul-poly a b))
(defmethod equ? ::polynomial [a b] (equ?-poly a b))

;this isn't a great decision to just pick a variable, but I don't think it will matter yet.
(defmethod raise :sicp.chapter-2.arithmetic.elements.complex-numbers/complex [a] (make-poly 'x (make-dense-termlist a)))
(defmethod number-project ::polynomial [a] (constant-term (term-list a)))
(derive :sicp.chapter-2.arithmetic.elements.complex-numbers/complex ::polynomial)

