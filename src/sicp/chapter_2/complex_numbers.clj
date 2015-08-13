(ns sicp.chapter-2.complex-numbers
  (:use sicp.chapter-2.type-tags)
  (:use clojure.math.numeric-tower))


(defmulti real-part (fn [c] (tag c)))
(defmulti imag-part (fn [c] (tag c)))
(defmulti magnitude (fn [c] (tag c)))
(defmulti angle (fn [c] (tag c)))


;polar representation
(defn make-from-mag-ang[magnitude angle]
  (attach-tag :polar [magnitude angle]))

(defmethod magnitude :polar [z] (first (content z)))
(defmethod angle :polar [z] (second (content z)))
(defmethod real-part :polar [z] (* (magnitude z) (java.lang.Math/cos (angle z))))
(defmethod imag-part :polar [z] (* (magnitude z) (java.lang.Math/sin (angle z))))

;cartesian representation
(defn make-from-real-imag[real imag]
  (attach-tag :real-imag [real imag]))

(defmethod real-part :real-imag [z] (first (content z)))
(defmethod imag-part :real-imag [z] (second (content z)))
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

