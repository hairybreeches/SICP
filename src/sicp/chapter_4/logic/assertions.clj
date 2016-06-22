(ns sicp.chapter-4.logic.assertions
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.database)
  (:use sicp.sequences)
  (:require [schema.core :as s]))

(def pattern-match)

(defn- extend-if-consistent
  [variable datum frame]
  (let [bind (binding-in-frame variable frame)]
    (if bind
      (pattern-match (binding-value bind) datum frame)
      (extend-frame variable datum frame))))

(defn- pattern-match
  [pattern datum frame]
  (cond
    (= frame 'failed) 'failed
    (= pattern datum) frame
    (variable? pattern) (extend-if-consistent pattern datum frame)
    (and (seq? pattern)
         (seq? datum)) (pattern-match (scheme-rest pattern)
                                      (rest datum)
                                      (pattern-match
                                        (first pattern)
                                        (first datum)
                                        frame))
    :else 'failed))

(s/defn check-an-assertion :- Frame-Stream
        [assertion query-pattern query-frame :- Frame]
  (let [match-result (pattern-match query-pattern assertion query-frame)]
    (if (= match-result 'failed)
        '()
        (list match-result))))

(s/defn find-assertions
        [pattern
         frame :- Frame]

  (mapcat
    (fn [datum] (check-an-assertion datum pattern frame))
    (fetch-assertions pattern frame)))

