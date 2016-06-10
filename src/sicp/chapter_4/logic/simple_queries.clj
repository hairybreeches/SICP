(ns sicp.chapter-4.logic.simple-queries
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.rules)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.database)
  (:use sicp.sequences)
  (:use sicp.chapter-4.logic.rule-stack)
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

(s/defn check-an-assertion
        [assertion
         query-pattern
         query-frame :- Frame
         success
         fail]

  (let [match-result (pattern-match query-pattern assertion query-frame)]
    (if (= match-result 'failed)
        (fail)
        (success match-result fail))))


(s/defn check-assertions
        [assertions
         pattern
         frame :- Frame
         success
         fail]
        (if (empty? assertions)
          (fail)
          (check-an-assertion
            (first assertions)
            pattern
            frame
            success
            (fn []
              (check-assertions
                (rest assertions)
                pattern
                frame
                success
                fail)))))

(s/defn find-assertions
        [pattern
         frame :- Frame
         success
         fail]

        (let [assertions (fetch-assertions pattern frame)]
          (check-assertions assertions pattern frame success fail)))

(s/defn analyse-simple-query
        [query-pattern]
        (s/fn
              [frame :- Frame
               rule-stack :- Rule-Stack
               success
               fail]
          (find-assertions
            query-pattern
            frame
            success
            (fn []
              (apply-rules
                query-pattern
                frame
                rule-stack
                success
                fail)))))

(defmethod analyse-dispatch :default [query-type query-pattern]
  (analyse-simple-query (cons query-type query-pattern)))

