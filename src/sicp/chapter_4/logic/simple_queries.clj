(ns sicp.chapter-4.logic.simple-queries
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.rules)
  (:use sicp.chapter-4.logic.frames)
  (:use sicp.chapter-4.logic.query-syntax)
  (:use sicp.chapter-4.logic.database))

(def pattern-match)

(defn- extend-if-consistent
  [variable datum frame]
  (let [bind (binding-in-frame variable frame)]
    (if bind
      (pattern-match (binding-value bind) datum frame)
      (exten variable datum frame))))

;todo: doesn't deal with dot notation
(defn- pattern-match
  [pattern datum frame]
  (cond
    (= frame 'failed) 'failed
    (= pattern datum) frame
    (variable? pattern) (extend-if-consistent pattern datum frame)
    (and (seq? pattern)
         (seq? datum)) (pattern-match (rest pattern)
                                      (rest datum)
                                      (pattern-match
                                        (first pattern)
                                        (first datum)))
    :else 'failed))

(defn- check-an-assertion [assertion query-pattern query-frame]
  (let [match-result (pattern-match query-pattern assertion query-frame)]
    (if (= match-result 'failed)
        '()
        (list match-result))))

(defn- find-assertions [pattern frame]
  (mapcat
    (fn [datum] (check-an-assertion datum pattern frame))
    (fetch-assertions pattern frame)))

(defmethod qeval :default [query-pattern frames]
  (mapcat
    #(concat
       (find-assertions query-pattern %)
       (apply-rules query-pattern %))
    frames))
