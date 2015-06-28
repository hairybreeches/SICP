(ns sicp.chapter-2.henderson-escher)

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

;frames

(defn origin-frame[frame]
  )

(defn edge1-frame[frame]
  )

(defn edge2-frame[frame]
  )

(defn frame-coord-map[frame]
  (fn [v]
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))


;painters
(defn flip-horiz[painter]
  )

(defn flip-vert[painter]
  )

(defn beside[left right]
  )

(defn below[bottom top]
  )

(defn rotate180[bottom top]
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
  (let [combine4 (square-of-four flip-horiz identity rotate180 flip-vert)]
       (combine4 (corner-split painter n))))



