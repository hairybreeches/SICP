(ns sicp.chapter-4.logic.unification
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.sequences)
  (:require [schema.core :as s]))

(defn- depends-on?
  [exp variable frame]
  (cond (variable? exp) (if (= variable exp)
                          true
                          (let [b (binding-in-frame exp frame)]
                            (if b
                              (depends-on? (binding-value b) variable frame)
                              false)))
        (non-empty-seq? exp) (or (depends-on? (first exp) variable frame)
                       (depends-on? (rest exp) variable frame))
        :else false))

(def unify-match)

(s/defn extend-if-possible
  [variable value frame :- Frame]
  (let [bind (binding-in-frame variable frame)]
    (cond bind (unify-match (binding-value bind) value frame)
          (variable? value) (let [bind2 (binding-in-frame value frame)]
                              (if bind2
                                (unify-match variable (binding-value bind2) frame)
                                (extend-frame variable value frame)))
          (depends-on? value variable frame) 'failed
          :else (extend-frame variable value frame))))

(defn unify-match
  [pattern1 pattern2 frame]
  (cond
    (= frame 'failed) 'failed
    (= pattern1 pattern2) frame
    (variable? pattern1) (extend-if-possible pattern1 pattern2 frame)
    (variable? pattern2) (extend-if-possible pattern2 pattern1 frame)
    (and (non-empty-seq? pattern1)
         (non-empty-seq? pattern2)) (unify-match
                            (scheme-rest pattern1)
                            (scheme-rest pattern2)
                            (unify-match
                              (first pattern1)
                              (first pattern2)
                              frame))
    :else 'failed))
