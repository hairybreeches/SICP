(ns sicp.chapter-2.interval-arithmetic
  (:use sicp.average))

;data layer: interval model

(defn make-interval [lower upper]
  (if
    (< upper lower)
    (throw (Exception. (format "Can't have an interval with a lower bound (%d) greater than the upper bound (%d)" lower upper))))

  [lower upper])

(defn make-centre-width[centre width]
  (make-interval (- centre width) (+ centre width)))

(defn upper-bound [interval]
  (second interval))

(defn lower-bound [interval]
  (first interval))

(defn centre[interval]
  (average (lower-bound interval) (upper-bound interval)))

(defn width[interval]
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(defn make-centre-percent[centre percent-tolerance]
  (make-centre-width centre (* percent-tolerance (/ centre 100))))

(defn percent-tolerance[interval]
  (* (/ (width interval) (centre interval)) 100))


;interval arithmetic
(defn spans-zero[interval]
  (<= (* (lower-bound interval)
        (upper-bound interval))
     0))

(defn add-interval [a b]
  (make-interval (+ (lower-bound a)
                    (lower-bound b))
                 (+ (upper-bound a)
                    (upper-bound b))))

(defn positive? [interval]
  (>= (lower-bound interval) 0))

(defn negative? [interval]
  (<= (upper-bound interval) 0))

(defn mul-pn[positive-interval negative-interval]
  (make-interval
    (* (upper-bound positive-interval) (lower-bound negative-interval))
    (* (lower-bound positive-interval) (upper-bound negative-interval))))

(defn mul-ps[positive-interval span-interval]
  (make-interval
    (* (upper-bound positive-interval) (lower-bound span-interval))
    (* (upper-bound positive-interval) (upper-bound span-interval))))

(defn mul-pp[positive-interval-a positive-interval-b]
  (make-interval
    (* (lower-bound positive-interval-a) (lower-bound positive-interval-b))
    (* (upper-bound positive-interval-a) (upper-bound positive-interval-b))))

(defn mul-ns[negative-interval span-interval]
  (make-interval
    (* (lower-bound negative-interval) (upper-bound span-interval))
    (* (lower-bound negative-interval) (lower-bound span-interval))))

(defn mul-nn[negative-interval-a negative-interval-b]
  (make-interval
    (* (upper-bound negative-interval-a) (upper-bound negative-interval-b))
    (* (lower-bound negative-interval-a) (lower-bound negative-interval-b))))

(defn mul-ss[span-interval-a span-interval-b]
  (let [attempts [(* (upper-bound span-interval-a) (upper-bound span-interval-b))
                  (* (upper-bound span-interval-a) (lower-bound span-interval-b))
                  (* (lower-bound span-interval-a) (upper-bound span-interval-b))
                  (* (lower-bound span-interval-a) (lower-bound span-interval-b))]]
    (make-interval
      (apply min attempts)
      (apply max attempts))))



(defn mul-interval [a b]
  (cond (positive? a) (cond (positive? b) (mul-pp a b)
                            (negative? b) (mul-pn a b)
                            :else (mul-ps a b))
        (negative? a) (cond (positive? b) (mul-pn b a)
                            (negative? b) (mul-nn a b)
                            :else (mul-ns a b))
        :else (cond         (positive? b) (mul-ps b a)
                            (negative? b) (mul-ns b a)
                            :else (mul-ss a b))))

(defn div-interval [a b]
  (if (spans-zero b)
    (throw (Exception. "cannot divide by an interval that spans zero")))
  (mul-interval a (make-interval (/ 1 (upper-bound b))
                                 (/ 1 (lower-bound b)))))

(defn sub-interval [a b]
  (add-interval a (make-interval (* -1 (upper-bound b))
                                 (* -1 (lower-bound b)))))

;functions using interval arithmetic
(defn par1[r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defn par2[r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval
      one
      (add-interval
        (div-interval one r1)
        (div-interval one r2)))))
