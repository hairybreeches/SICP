(ns sicp.chapter-2.arithmetic.elements.polynomials
  (:use clojure.math.numeric-tower)
  (:use sicp.chapter-2.arithmetic.arithmetic-operations)
  (:use sicp.chapter-2.arithmetic.numerical-type-system)
  (:use sicp.chapter-2.arithmetic.elements.rational-numbers))

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
(defmulti coefficients get-format)


(defn mul-terms [l1 l2]
  (if (empty-termlist? l1) l1
      (add-terms (mul-term-by-all-terms l2 (term-list-order l1) (get-leading-coefficient l1))
                 (mul-terms (rest-terms l1) l2))))

(defmethod variables ::termlist [termlist]
  (mapcat variables (coefficients termlist)))

(defn sub-terms[t1 t2]
  (add-terms t1 (mul-term-by-all-terms t2 0 -1)))

;sparse termlist representation
(defn make-term[order coeff]
  (list order coeff))

(defn coeff [term]
  (second term))

(defn order [term]
  (first term))

(defmethod term-list-order :sparse [l]
  (if (empty? l)
      0
      (order (first-term l))))

(defmethod get-leading-coefficient :sparse [l]
  (coeff (first-term l)))

(defmethod constant-term :sparse [l]
  (let [term (last-term l)]
    (if (and
           (not (nil? term))
           (= 0 (order term)))
        (coeff term)
        0)))

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
    (apply list (filter #(not (equ? 0 (coeff %))) args))
    {:format :sparse :type ::termlist}))


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

(defmethod coefficients :sparse [termlist]
  (map coeff termlist))

;dense termlist representation

(defn make-dense-termlist[& terms]
    (with-meta
      (apply list (drop-while #(equ? % 0) terms))
      {:format :dense :type ::termlist}))

(defmethod term-list-order :dense [termlist]
  (max (dec (count termlist)) 0))

(defmethod get-leading-coefficient :dense [termlist]
  (first termlist))

(defmethod constant-term :dense [l]
  (let [coefficient (last-term l)]
    (if (nil? coefficient)
        0
      coefficient)))

(defmethod term-list= :dense [l m] (= l m)
  (loop [terms1 l
         terms2 m]
    (cond (empty? terms2) (empty? terms1)
          (empty? terms1) false
          (equ? (first-term terms1) (first-term terms1)) (recur (rest-terms terms1) (rest-terms terms2))
          :else false)))


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

(defmethod coefficients :dense [termlist]
  termlist)

;mixed operations
(defmethod add-terms :mixed [& args]
  (apply add-terms (map to-sparse-format args)))

(defmethod term-list= :mixed [& args]
  (apply term-list= (map to-sparse-format args)))

;polynomials
(defn term-list [poly]
  (:terms poly))

(defn variable [poly]
  (:variable poly))

(defn variable-gt[a b]
  (> (compare a b) 0))

(defn illegal-variables[variable terms]
  (filter
   #(variable-gt variable %)
   (get-variables terms)))

(defn make-poly [var terms]
    (let [illegal (illegal-variables var terms)]
      (if (not (empty? illegal))
          (throw (Exception. (str "Cannot create a polynomial in " var " with coefficients polynomials in " (apply list illegal))))
          ^{:type ::polynomial}
          {:variable var :terms terms})))

(defn polynomial-order[poly]
  (term-list-order (term-list poly)))

(defn to-shared-variable [p1 p2]
  (cond (= (variable p1) (variable p2)) (list p1 p2)
        (= (polynomial-order p2) 0) (list p1 (make-poly (variable p1) (term-list p2)))
        (= (polynomial-order p1) 0) (list (make-poly (variable p2) (term-list p1)) p2)
        (variable-gt (variable p2)
                     (variable p1)) (list p1 (make-poly (variable p1) (make-dense-termlist p2)))
        (variable-gt (variable p1)
                     (variable p2)) (list (make-poly (variable p2) (make-dense-termlist p1)) p2)
        :else (throw (Exception. "WTF?"))))

(defn div-terms [l1 l2]
  (if (empty-termlist? l1) [(make-sparse-termlist) (make-sparse-termlist)]
      (let [l1-order (term-list-order l1)
            l2-order (term-list-order l2)]
      (if (> l2-order l1-order)
          [(make-sparse-termlist) l1]
          (let [new-term-coefficient (div (get-leading-coefficient l1) (get-leading-coefficient l2))
                new-term-order (- l1-order l2-order)
                new-term (make-term new-term-order new-term-coefficient)
                result (make-sparse-termlist new-term)]
            (let [rest-result (div-terms
                               (sub-terms l1 (mul-terms result l2))
                                 l2)]
              [(adjoin-term new-term (first rest-result)) (second rest-result)]
            ))))))

(defn pseudoremainder-terms[t1 t2]
  (let [multiplying-factor (java.lang.Math/pow (get-leading-coefficient t2) (inc (- (term-list-order t1) (term-list-order t2))))]
    (second (apply div-terms (map #(mul-term-by-all-terms % 0 multiplying-factor) [t1 t2])))))

(defn reduce-coefficients[& termlists]
  (let [divisor (reduce gcd (mapcat coefficients termlists))]
    (map #(mul-term-by-all-terms % 0 (make-rat 1 divisor)) termlists)))

(defn gcd-terms[t1 t2]
  (if (empty-termlist? t2)
      (first (reduce-coefficients t1))
    (gcd-terms t2 (pseudoremainder-terms t1 t2))))

(defn get-integerising-factor[gcd n d]
  (let [o1 (max (term-list-order n) (term-list-order d))
        o2 (term-list-order gcd)]
    (java.lang.Math/pow
       (get-leading-coefficient gcd)
       (inc (- o1 o2)))))

(defn strict-divide[n d]
  (let [[result remainder] (div-terms n d)]
    (if (not (empty? remainder))
        (throw (Exception. "Unexpected quotient"))
        result)))

(defn reduce-terms[numer denom]
  (let [gcd (gcd-terms numer denom)
        integerising-factor (get-integerising-factor gcd numer denom)]
    (apply reduce-coefficients
    (map #(strict-divide
           (mul-term-by-all-terms % 0 integerising-factor)
           gcd) [numer denom]))))

(defn reduce-poly[q1 q2]
  (let [[p1 p2] (to-shared-variable q1 q2)]
    (map #(make-poly (variable p1) %) (apply reduce-terms (map term-list [p1 p2])))))

(defn gcd-poly[q1 q2]
  (let [[p1 p2] (to-shared-variable q1 q2)]
    (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))))


(defn add-poly [q1 q2]
  (let [[p1 p2] (to-shared-variable q1 q2)]
    (make-poly (variable p1)
               (add-terms (term-list p1)
                          (term-list p2)))))

(defn mul-poly [q1 q2]
  (let [[p1 p2] (to-shared-variable q1 q2)]
    (make-poly (variable p1)
               (mul-terms (term-list p1)
                          (term-list p2)))))

(defn div-poly[q1 q2]
  (let [[p1 p2] (to-shared-variable q1 q2)]
    (map (partial make-poly (variable p1)) (div-terms (term-list p1) (term-list p2)))))


(defn equ?-poly[a b]
  (and (or (= (variable a) (variable b))
           (= 0 (polynomial-order a))
           (= 0 (polynomial-order b)))
       (term-list= (term-list a) (term-list b))))

(defmethod add-pair ::polynomial [a b] (add-poly a b))
(defmethod mul-pair ::polynomial [a b] (mul-poly a b))
(defmethod equ? ::polynomial [a b] (equ?-poly a b))
(defmethod variables ::polynomial [p] (cons (variable p) (variables (term-list p))))
(defmethod greatest-common-divisor ::polynomial [a b] (gcd-poly a b))
(defmethod reduce-quotient ::polynomial [a b] (reduce-poly a b))


;this isn't a great decision to just pick a variable, but I don't think it will matter yet.
(defmethod raise :sicp.chapter-2.arithmetic.elements.complex-numbers/complex [a] (make-poly 'x (make-dense-termlist a)))
(defmethod number-project ::polynomial [a] (constant-term (term-list a)))
(derive :sicp.chapter-2.arithmetic.elements.complex-numbers/complex ::polynomial)

