(ns sicp.test.chapter-2.henderson-escher
  (:use sicp.chapter-2.henderson-escher)
  (:use clojure.test))

;vectors

(deftest can-scale-vectors
  (is (= (scale-vect 3 (make-vect 2 1)) (make-vect 6 3))))

(deftest can-add-vectors
  (is (= (add-vect (make-vect 3 1) (make-vect 2 1)) (make-vect 5 2))))

(deftest can-subtract-vectors
  (is (= (sub-vect (make-vect 3 1) (make-vect 2 1)) (make-vect 1 0))))

;frames

(defn test-frame-model[constructor origin-selector edge1-selector edge2-selector]
  (let [origin (make-vect 3 1)
        edge1 (make-vect 2 4)
        edge2 (make-vect 1 4)
        frame (constructor origin edge1 edge2)]
    (is (= (origin-selector frame) origin))
    (is (= (edge1-selector frame) edge1))
    (is (= (edge2-selector frame) edge2))))

(deftest list-frame-model-correct
  (test-frame-model make-frame origin-frame edge1-frame edge2-frame))

(deftest pairs-frame-model-correct
  (test-frame-model make-frame-pair origin-frame-pair edge1-frame-pair edge2-frame-pair))

(def whole-canvas
  (make-frame bottom-left bottom-right top-left))

(def bottom-left-quarter
  (make-frame (make-vect 0.0 0.0) (make-vect 0.5 0.0) (make-vect 0.0 0.5)))

(def top-left-quarter
  (make-frame (make-vect 0.0 0.5) (make-vect 0.5 0.0) (make-vect 0.0 0.5)))

(def funky-paralellogram
  (make-frame (make-vect 0.5 0.5) (make-vect 0.25 0.0) (make-vect 0.25 0.5)))

;when the frame is the whole canvas, the mapper should be the identity
(deftest test-vector-mapping-with-whole-canvas
  (let [mapper (frame-coord-map whole-canvas)]
    (is (= (mapper (make-vect 0.0 0.0)) (make-vect 0.0 0.0)))
    (is (= (mapper (make-vect 0.5 0.0)) (make-vect 0.5 0.0)))
    (is (= (mapper (make-vect 0.0 1.0)) (make-vect 0.0 1.0)))
    (is (= (mapper (make-vect 1.0 0.8)) (make-vect 1.0 0.8)))))

;when the frame is the bottom left quadrant, the mapper should multiply everything by half.
(deftest test-vector-mapping-with-bottom-left-quarter
  (let [mapper (frame-coord-map bottom-left-quarter)]
    (is (= (mapper (make-vect 0.0 0.0)) (make-vect 0.0 0.0)))
    (is (= (mapper (make-vect 0.5 0.0)) (make-vect 0.25 0.0)))
    (is (= (mapper (make-vect 0.0 1.0)) (make-vect 0.0 0.5)))
    (is (= (mapper (make-vect 1.0 0.8)) (make-vect 0.5 0.4)))))


;when the frame is the top left quadrant, the mapper should multiply everything by half and then add (0, 0.5)
(deftest test-vector-mapping-with-top-left-quarter
  (let [mapper (frame-coord-map top-left-quarter)]
    (is (= (mapper (make-vect 0.0 0.0)) (make-vect 0.0 0.5)))
    (is (= (mapper (make-vect 0.5 0.0)) (make-vect 0.25 0.5)))
    (is (= (mapper (make-vect 0.0 1.0)) (make-vect 0.0 1.0)))
    (is (= (mapper (make-vect 1.0 0.8)) (make-vect 0.5 0.9)))))

(defn expected-line-string[from to]
  (string-concat "\"" (line-to-string from to) "\"\r\n"))

(defn expected-box-string[bl tl tr br]
  (clojure.string/join  [(expected-line-string bl tl)
              (expected-line-string tl tr)
              (expected-line-string tr br)
              (expected-line-string br bl)]))

(defn get-print-out[painter frame]
  (with-out-str (painter frame)))

(deftest test-frame-outline-drawer
  (is (= (get-print-out frame-outline-painter whole-canvas) (expected-box-string [0.0 0.0] [0.0 1.0] [1.0 1.0] [1.0 0.0])))
  (is (= (get-print-out frame-outline-painter top-left-quarter) (expected-box-string [0.0 0.5] [0.0 1.0] [0.5 1.0] [0.5 0.5])))
  (is (= (get-print-out frame-outline-painter funky-paralellogram) (expected-box-string [0.5 0.5] [0.75 1.0] [1.0 1.0] [0.75 0.5]))))

