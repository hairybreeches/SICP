(ns sicp.chapter-4.logic.and
  (:use sicp.chapter-4.logic.evaluation)
  (:use sicp.chapter-4.logic.unification))

(defn- combine-with-stream [frame stream]
  (filter #(not (= 'failed %))
          (map #(unify-frames frame %) stream)))

(defn- combine-and-streams [stream1 stream2]
  (mapcat #(combine-with-stream % stream2) stream1))


(defn- conjoin [conjuncts frames rule-stack]
  (reduce combine-and-streams (map #(qeval % frames rule-stack) conjuncts)))

(defmethod qeval-dispatch 'and [_ query-pattern frames rule-stack]
  (conjoin query-pattern frames rule-stack))

