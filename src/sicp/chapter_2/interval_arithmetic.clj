(ns sicp.chapter-2.interval-arithmetic)

(defn make-interval [lower upper]
  (if
    (< upper lower)
    (throw (Exception. (format "Can't have an interval with a lower bound (%d) greater than the upper bound (%d)" lower upper))))

  [lower upper])

(defn upper-bound [interval]
  (second interval))

(defn lower-bound [interval]
  (first interval))

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

(defn mul-interval [a b]
  (cond (positive? a) (cond (positive? b) (make-interval
                                           (* (lower-bound a) (lower-bound b))
                                           (* (upper-bound a) (upper-bound b)))
                            (negative? b) (make-interval
                                           (* (upper-bound a) (lower-bound b))
                                           (* (lower-bound a) (upper-bound b)))
                            :else         (make-interval
                                           (* (upper-bound a) (lower-bound b))
                                           (* (upper-bound a) (upper-bound b))))
        (negative? a) (cond (positive? b) (make-interval
                                           (* (lower-bound a) (upper-bound b))
                                           (* (upper-bound a) (lower-bound b)))
                            (negative? b) (make-interval
                                           (* (upper-bound a) (upper-bound b))
                                           (* (lower-bound a) (lower-bound b)))
                            :else         (make-interval
                                           (* (lower-bound a) (upper-bound b))
                                           (* (lower-bound a) (lower-bound b))))
        :else (cond         (positive? b) (make-interval
                                           (* (lower-bound a) (upper-bound b))
                                           (* (upper-bound a) (upper-bound b)))
                            (negative? b) (make-interval
                                           (* (upper-bound a) (lower-bound b))
                                           (* (lower-bound a) (lower-bound b)))
                            :else (let [attempts [(* (upper-bound a) (upper-bound b))
                                                  (* (upper-bound a) (lower-bound b))
                                                  (* (lower-bound a) (upper-bound b))
                                                  (* (lower-bound a) (lower-bound b))]]
                                    (make-interval (apply min attempts)
                                                   (apply max attempts))))))

(defn div-interval [a b]
  (if (spans-zero b)
    (throw (Exception. "cannot divide by an interval that spans zero")))
  (mul-interval a (make-interval (/ 1 (upper-bound b))
                                 (/ 1 (lower-bound b)))))

(defn sub-interval [a b]
  (add-interval a (make-interval (* -1 (upper-bound b))
                                 (* -1 (lower-bound b)))))
