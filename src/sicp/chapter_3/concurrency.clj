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


(defn- test-and-set-number!
  [n max-n]
  (dosync
   (if (< (ensure n) max-n)
       (do
         (alter n inc)
         false)
       true)))

(defn- release!
  [n]
  (dosync
   (if (> @n 0)
     (alter n dec)
     (throw (Exception. "Can't release unacquired semaphore!")))))

(defn make-semaphore-count
  [max-n on-failure]
  (let [cell (ref 0)

        the-semaphore (ref false)

        dispatch (fn [m]
          (cond (= m :acquire)
                (if (test-and-set-number! cell max-n)
                    (do
                      (on-failure the-semaphore)
                      (the-semaphore :acquire)))

                (= m :release)
                (release! cell)))]
    (dosync
     (ref-set the-semaphore dispatch)
    the-semaphore)))
