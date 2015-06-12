(ns sicp.test.find-roots
  (:use sicp.chapter-1.ex-45)
  (:require sicp.test.accuracy)
  (:use clojure.test))

(defn is-roughly=[a b]
  (sicp.test.accuracy/is-roughly= a b 0.01))


(deftest can-find-square-roots
  (let [root #(find-root % 2)]
    (is-roughly= (root 4) 2)
    (is-roughly= (root 9) 3)
    (is-roughly= (root 16) 4)
    (is-roughly= (root 25) 5)))

(deftest can-find-cube-roots
  (let [root #(find-root % 3)]
    (is-roughly= (root 8) 2)
    (is-roughly= (root 27) 3)
    (is-roughly= (root 64) 4)
    (is-roughly= (root 125) 5)))


(deftest can-find-fourth-roots
  (let [root #(find-root % 4)]
    (is-roughly= (root 16) 2)
    (is-roughly= (root 91) 3)
    (is-roughly= (root 256) 4)
    (is-roughly= (root 625) 5)))


(deftest can-find-fifth-roots
  (let [root #(find-root % 5)]
    (is-roughly= (root 32) 2)
    (is-roughly= (root 273) 3)
    (is-roughly= (root 1024) 4)
    (is-roughly= (root 3125) 5)))

(deftest can-find-sixth-roots
  (let [root #(find-root % 6)]
    (is-roughly= (root 64) 2)
    (is-roughly= (root 819) 3)
    (is-roughly= (root 4096) 4)
    (is-roughly= (root 15625) 5)))

(deftest can-find-seventh-roots
  (let [root #(find-root % 7)]
    (is-roughly= (root 128) 2)
    (is-roughly= (root 2457) 3)
    (is-roughly= (root 16384) 4)
    (is-roughly= (root 78125) 5)))

(deftest can-find-eighth-roots
  (let [root #(find-root % 8)]
    (is-roughly= (root 256) 2)
    (is-roughly= (root 7371) 3)
    (is-roughly= (root 65536) 4)
    (is-roughly= (root 390625) 5)))
