(ns sicp.chapter-2.arithmetic.elements.complex-numbers
  (:use clojure.math.numeric-tower)
  (:use sicp.chapter-2.arithmetic.arithmetic-operations)
  (:use sicp.chapter-2.arithmetic.numerical-type-system)
  (:use sicp.chapter-2.arithmetic.elements.real-numbers))

(def real (partial convert-to-type :sicp.chapter-2.arithmetic.elements.real-numbers/real))

(defmulti real-part get-format)
(defmulti imag-part get-format)
(defmulti magnitude get-format)
(defmulti angle get-format)


;polar representation
(defn make-from-mag-ang[magnitude angle]
  ^{:type ::complex :format ::polar}
  {:magnitude (simplify magnitude) :angle (simplify angle)})

(defmethod magnitude ::polar [z] (:magnitude z))
(defmethod angle ::polar [z] (:angle z))
(defmethod real-part ::polar [z] (mul (magnitude z) (java.lang.Math/cos (real (angle z)))))
(defmethod imag-part ::polar [z] (mul (magnitude z) (java.lang.Math/sin (real (angle z)))))

;cartesian representation
(defn make-from-real-imag[real imag]
  ^{:type ::complex :format ::real-imag}
  {:real (simplify real) :imag (simplify imag)})

(defmethod real-part ::real-imag [z] (:real z))
(defmethod imag-part ::real-imag [z] (:imag z))
(defmethod magnitude ::real-imag [z] (sqrt (real (add (mul (real-part z) (real-part z)) (mul (imag-part z) (imag-part z))))))
(defmethod angle ::real-imag [z] (java.lang.Math/atan2 (real (imag-part z)) (real (real-part z))))

;interface methods
(defn add-complex[a b]
  (make-from-real-imag (add (real-part a) (real-part b))
                       (add (imag-part a) (imag-part b))))

(defn mul-complex[a b]
  (make-from-mag-ang (mul (magnitude a) (magnitude b))
                     (add (angle a) (angle b))))

(defn div-complex[a b]
  (make-from-mag-ang (div (magnitude a) (magnitude b))
                     (sub (angle a) (angle b))))

(defn equ?-complex[a b]
  (and (equ? (real-part a) (real-part b))
       (equ? (imag-part a) (imag-part b))))

(defmethod add-pair ::complex [a b] (add-complex a b))
(defmethod mul-pair ::complex [a b] (mul-complex a b))
(defmethod div-pair ::complex [a b] (div-complex a b))
(defmethod equ? ::complex [a b] (equ?-complex a b))
(defmethod greatest-common-divisor ::complex [a b] (throw (Exception. "Cannot calculate gcd for complex numbers")))
(defmethod reduce-quotient ::complex [a b] (throw (Exception. "Cannot reduce quotient for complex numbers")))

(defmethod raise :sicp.chapter-2.arithmetic.elements.real-numbers/real [a] (make-from-real-imag a 0))
(defmethod number-project ::complex [a] (real-part a))
(defmethod variables ::complex [a] (mapcat variables (list (real-part a) (imag-part a))))
(derive :sicp.chapter-2.arithmetic.elements.real-numbers/real ::complex)

