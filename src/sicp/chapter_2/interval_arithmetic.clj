(ns sicp.chapter-2.interval-arithmetic)

(defn make-interval [lower upper]
  [lower upper])

(defn upper-bound [interval]
  (second interval))

(defn lower-bound [interval]
  (first interval))

(defn add-interval [a b]
  (make-interval (+ (lower-bound a)
                    (lower-bound b))
                 (+ (upper-bound a)
                    (upper-bound b))))

(defn mul-interval [a b]
  (let [attempts [(* (upper-bound a) (upper-bound b))
                  (* (upper-bound a) (lower-bound b))
                  (* (lower-bound a) (upper-bound b))
                  (* (lower-bound a) (lower-bound b))]
        ]
    (make-interval (apply min attempts)
                   (apply max attempts))))

(defn div-interval [a b]
  (mul-interval a (make-interval (/ 1 (upper-bound b))
                                 (/ 1 (lower-bound a)))))

(defn sub-interval [a b]
  (add-interval a (make-interval (* -1 (upper-bound b))
                                 (* -1 (lower-bound a)))))
