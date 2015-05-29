(ns sicp.1-46)

(defn iterative-improve [good-enough? improve-guess]
  (fn [first-guess]
    (loop [guess first-guess]
      (let [next-guess (improve-guess guess)]
        (if (good-enough? guess next-guess)
          next-guess
          (recur next-guess))))))
