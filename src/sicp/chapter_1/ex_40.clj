(ns sicp.chapter-1.ex-40
  (:use sicp.chapter-1.ex-35))

(def dx 0.00001)

(defn deriv[f]
  #(/ (- (f (+ % dx)) (f %)) dx))

(defn newton-transform [f]
  #(- % (/ (f %) ((deriv f) %))))

(defn newtons-method [f guess tolerance]
  (fixed-point (newton-transform f) guess tolerance))

(defn cubic [a b c]
  #(let [square (* % %)]
     (+ (* square %)
        (* a square)
        (* b %)
        c)))








