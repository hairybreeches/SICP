(ns sicp.test.chapter-4.multiple-dwelling-comparison
  (:use clojure.test))

(defn exclude
  [options & to-exclude]
  (let [exclude-set (apply hash-set to-exclude)]
   (filter
     #(not (exclude-set %))
     options)))

(defn baker-options []
  '(1 2 3 4))

(defn cooper-options [so-far]
  (let [baker (:baker so-far)]
    (exclude '(2 3 4 5) baker)))

(defn fletcher-options [so-far]
  (let [baker (:baker so-far)
        cooper (:cooper so-far)]
  (exclude '(2 3 4) baker cooper (+ cooper 1) (- cooper 1))))

(defn miller-options [so-far]
  (let [baker (:baker so-far)
        cooper (:cooper so-far)
        fletcher (:fletcher so-far)]
  (filter
    #(> % cooper)
    (exclude '(1 2 3 4 5) baker cooper fletcher))))

(defn smith-options [so-far]
  (let [baker (:baker so-far)
        cooper (:cooper so-far)
        fletcher (:fletcher so-far)
        miller (:miller so-far)]
  (exclude '(1 2 3 4 5) baker cooper fletcher miller (+ fletcher 1) (- fletcher 1))))

(defn add-options [so-far n options]
  (map #(assoc so-far n %) options))

(defn refine-options [generator n options-so-far]
  (mapcat #(add-options % n (generator %)) options-so-far))

(defn multiple-dwelling []
  (->>
    (baker-options)
    (map (fn [%] {:baker %}))
    (refine-options cooper-options :cooper)
    (refine-options fletcher-options :fletcher)
    (refine-options miller-options :miller)
    (refine-options smith-options :smith)))

(deftest multiple-dwelling-clojure
  (is (= (multiple-dwelling) '({:baker 3 :cooper 2 :fletcher 4 :miller 5 :smith 1}))))
