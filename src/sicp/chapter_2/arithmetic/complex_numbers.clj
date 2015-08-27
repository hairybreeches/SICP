(ns sicp.chapter-2.arithmetic.complex-numbers
  (:use clojure.math.numeric-tower)
  (:use sicp.chapter-2.arithmetic.real-numbers)
  (:use sicp.chapter-2.arithmetic.universal-arithmetic))

(defn get-format[object]
  (:format (meta object)))

(defmulti real-part get-format)
(defmulti imag-part get-format)
(defmulti magnitude get-format)
(defmulti angle get-format)


;polar representation
(defn make-from-mag-ang[magnitude angle]
  ^{:type ::complex :format ::polar}
  {:magnitude magnitude :angle angle})

(defmethod magnitude ::polar [z] (:magnitude z))
(defmethod angle ::polar [z] (:angle z))
(defmethod real-part ::polar [z] (* (magnitude z) (java.lang.Math/cos (angle z))))
(defmethod imag-part ::polar [z] (* (magnitude z) (java.lang.Math/sin (angle z))))

;cartesian representation
(defn make-from-real-imag[real imag]
  ^{:type ::complex :format ::real-imag}
  {:real real :imag imag})

(defmethod real-part ::real-imag [z] (:real z))
(defmethod imag-part ::real-imag [z] (:imag z))
(defmethod magnitude ::real-imag [z] (sqrt (+ (expt (real-part z) 2) (expt (imag-part z) 2))))
(defmethod angle ::real-imag [z] (java.lang.Math/atan2 (imag-part z) (real-part z)))

;interface methods
(defn add-complex[a b]
  (make-from-real-imag (+ (real-part a) (real-part b))
                       (+ (imag-part a) (imag-part b))))

(defn sub-complex[a b]
  (make-from-real-imag (- (real-part a) (real-part b))
                       (- (imag-part a) (imag-part b))))

(defn mul-complex[a b]
  (make-from-mag-ang (* (magnitude a) (magnitude b))
                     (+ (angle a) (angle b))))

(defn div-complex[a b]
  (make-from-mag-ang (/ (magnitude a) (magnitude b))
                     (- (angle a) (angle b))))

(defn equ?-complex[a b]
  (and (equ? (real-part a) (real-part b))
       (equ? (imag-part a) (imag-part b))))

(defmethod add-pair ::complex [a b] (add-complex a b))
(defmethod sub-pair ::complex [a b] (sub-complex a b))
(defmethod mul-pair ::complex [a b] (mul-complex a b))
(defmethod div-pair ::complex [a b] (div-complex a b))
(defmethod equ? ::complex [a b] (equ?-complex a b))

(defmethod raise :sicp.chapter-2.arithmetic.real-numbers/real [a] (make-from-real-imag a 0))
(defmethod number-project ::complex [a] (real-part a))
(derive :sicp.chapter-2.arithmetic.real-numbers/real ::complex)

