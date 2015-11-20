(ns sicp.chapter-3.concurrency)

(defn- clear!
  [cell]
  (dosync
   (if @cell

      (do
        (ref-set cell false)
        true)

      false)))

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
                (clear! cell)

                (= m :try-acquire)
                (not (test-and-set! cell))))]
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


(defn- acquire-a-mutex
  [mutexes]
  (loop [mutexes mutexes]
    (cond (empty? mutexes)
          true

          ((first mutexes) :try-acquire)
          false

          :else
          (recur (rest mutexes)))))

(defn- release-a-mutex
  [mutexes]
  (loop [mutexes mutexes]
    (cond (empty? mutexes)
          (throw (Exception. "No acquired mutexes!"))

          (not ((first mutexes) :release))
          (recur (rest mutexes)))))


(defn make-semaphore-mutex
  [max-n on-failure]
  (let [mutexes (->>(iterate (fn [_] (make-mutex (fn[mutex]))) 0)
                    (drop 1)
                    (take max-n))


        the-semaphore (ref false)

        dispatch (fn [m]
          (cond (= m :acquire)
                (if (acquire-a-mutex mutexes)
                    (do
                      (on-failure the-semaphore)
                      (the-semaphore :acquire)))

                (= m :release)
                (release-a-mutex mutexes)))]
    (dosync
     (ref-set the-semaphore dispatch)
    the-semaphore)))























