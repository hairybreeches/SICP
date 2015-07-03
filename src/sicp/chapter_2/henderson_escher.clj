(ns sicp.chapter-2.henderson-escher
  (:use sicp.average)
  (:use sicp.chapter-2.pairs)
  (:use sicp.chapter-2.sequences))

(defn string-concat[& args]
  (clojure.string/join args))

;vectors
(defn make-vect[x-cor y-cor]
  [x-cor y-cor])

(defn xcor-vect[v]
  (first v))

(defn ycor-vect[v]
  (second v))

(defn scale-vect [s v]
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(defn add-vect [v w]
  (make-vect (+ (xcor-vect v) (xcor-vect w))
             (+ (ycor-vect v) (ycor-vect w))))

(defn sub-vect [v w]
  (add-vect
     v
     (scale-vect -1 w)))

(defn make-segment[v w]
  (list v w))

(defn start-segment[segment]
  (first segment))

(defn end-segment[segment]
  (second segment))

(defn to-string-vect[v]
  (string-concat "[" (xcor-vect v) ", " (ycor-vect v) "]"))

(defn average-vect[& args]
  (make-vect (apply average (map xcor-vect args))
             (apply average (map ycor-vect args))))

;frames

(defn make-frame[origin edge1 edge2]
  (list origin edge1 edge2))

(defn origin-frame[frame]
  (first frame))

(defn edge1-frame[frame]
  (first (rest frame)))

(defn edge2-frame[frame]
  (first (rest (rest frame))))

(defn make-frame-pair[origin edge1 edge2]
  (func-cons origin (func-cons edge1 edge2)))

(defn origin-frame-pair[frame]
  (func-car frame))

(defn edge1-frame-pair[frame]
  (func-car (func-cdr frame)))

(defn edge2-frame-pair[frame]
  (func-cdr (func-cdr frame)))

(defn frame-coord-map[frame]
  (fn [v]
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))


;painters
(defn line-to-string[start-vector end-vector]
  (string-concat (to-string-vect start-vector) " to " (to-string-vect end-vector)))

(defn draw-line[start-vector end-vector]
  (prn (line-to-string start-vector end-vector)))

(defn segments->painter[segment-list]
  (fn[frame]
    (let [mapper (frame-coord-map frame)]
      (for-each
        (fn [segment]
          (draw-line
           (mapper (start-segment segment))
           (mapper (end-segment segment))))
        segment-list))))

(def bottom-left (make-vect 0.0 0.0))
(def top-left (make-vect 0.0 1.0))
(def bottom-right (make-vect 1.0 0.0))
(def top-right (make-vect 1.0 1.0))


(def frame-outline-painter
  (segments->painter
   (list (make-segment bottom-left top-left)
         (make-segment top-left top-right)
         (make-segment top-right bottom-right)
         (make-segment bottom-right bottom-left))))

(def cross-painter
  (segments->painter
   (list (make-segment bottom-left top-right)
         (make-segment top-left bottom-right))))

(def backslash-painter
  (segments->painter
   (list (make-segment bottom-right top-left))))

(def diamond-painter
  (let [top (average-vect top-left top-right)
        left (average-vect bottom-left top-left)
        bottom (average-vect bottom-left bottom-right)
        right (average-vect bottom-right top-right)]

  (segments->painter
   (list (make-segment top right)
         (make-segment right bottom)
         (make-segment bottom left)
         (make-segment left top)))))

(def wave-painter
  (fn [frame] (throw (Exception. "You are surely taking the piss."))))

;transformations
(defn transform-painter[painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter (make-frame new-origin
                           (sub-vect (m corner1) new-origin)
                           (sub-vect (m corner2) new-origin))))))

(defn rotate-180[painter]
  (transform-painter painter
                     top-right
                     top-left
                     bottom-right))

(defn rotate-270[painter]
  (transform-painter painter
                     top-left
                     bottom-left
                     top-right))


(defn flip-horiz[painter]
  (transform-painter painter
                     top-left
                     bottom-left
                     top-right))

(defn flip-vert[painter]
  (transform-painter painter
                     top-left
                     top-right
                     bottom-left))

(defn beside[left right]
  (let [split-point (make-vect 0.5 0.0)
        paint-left (transform-painter left
                                      bottom-left
                                      split-point
                                      top-left)
        paint-right (transform-painter right
                                       split-point
                                       bottom-right
                                       (make-vect 0.5 1.0))]
    (fn [frame]
      (paint-left frame)
      (paint-right frame))))

(defn below[bottom top]
  )


(defn split[outer-transform inner-transform]
 (fn split-return[painter n]
    (if (= n 0)
      painter
       (let [smaller (split-return painter (dec n))]
         (outer-transform painter (inner-transform smaller smaller))))))

(def right-split
  (split beside below))

(def up-split
  (split below beside))

(defn corner-split[painter n]
  (if (= n 0)
    painter
    (let [up (up-split painter (dec n))
          right (right-split painter (dec n))
          top-left (beside up up)
          bottom-right (below right right)
          corner (corner-split painter (dec n))]
      (beside (below painter top-left)
              (below bottom-right corner)))))

(defn square-of-four[tl tr bl br]
  (fn [painter]
    (let [top (beside (tl painter) (tr painter))
          bottom (beside (bl painter) (br painter))]
      (below bottom top))))

(defn square-limit[painter n]
  (let [combine4 (square-of-four flip-horiz identity rotate-180 flip-vert)]
       (combine4 (corner-split painter n))))



