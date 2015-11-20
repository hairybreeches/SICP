(ns sicp.chapter-3.concurrency)

(defn- clear!
  [cell]
  (dosync (ref-set cell false)))

(defn- test-and-set!
  [cell]
  (dosync
   (if (ensure cell)
        true
       (dosync
         (ref-set cell true)
        false))))



(defn make-mutex
  [on-failure]
  (let [cell (ref false)

        the-mutex (ref false)

        dispatch (fn [m]
          (cond (= m :acquire)
                (if (test-and-set! cell)
                    (do
                      (on-failure the-mutex)
                      (the-mutex :acquire)))

                (= m :release)
                (clear! cell)))]
    (dosync
     (ref-set the-mutex dispatch)
    the-mutex)))
