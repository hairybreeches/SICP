(ns sicp.chapter-2.complex-numbers
  (:use clojure.math.numeric-tower))


(defmulti real-part type)
(defmulti imag-part type)
(defmulti magnitude type)
(defmulti angle type)


;polar representation
(defn make-from-mag-ang[magnitude angle]
  ^{:type :polar}
  {:magnitude magnitude :angle angle})

(defmethod magnitude :polar [z] (:magnitude z))
(defmethod angle :polar [z] (:angle z))
(defmethod real-part :polar [z] (* (magnitude z) (java.lang.Math/cos (angle z))))
(defmethod imag-part :polar [z] (* (magnitude z) (java.lang.Math/sin (angle z))))

;cartesian representation
(defn make-from-real-imag[real imag]
  ^{:type :real-imag}
  {:real real :imag imag})

(defmethod real-part :real-imag [z] (:real z))
(defmethod imag-part :real-imag [z] (:imag z))
(defmethod magnitude :real-imag [z] (sqrt (+ (expt (real-part z) 2) (expt (imag-part z) 2))))
(defmethod angle :real-imag [z] (java.lang.Math/atan2 (imag-part z) (real-part z)))

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

