(ns sicp.1-29)

(defn cube [x]
  (* x x x))

(defn sum [term start incrementor finish]
    (if (> start finish)
      0
      (+ (term start)
         (sum term (incrementor start) incrementor finish))))

(defn integral [f from to dx]
  (defn add-dx [x] (+ x dx))
  (* (sum f (+ from (/ dx 2.0)) add-dx to)
     dx))

(defn non-y-coefficient[x from to h]
  (cond (= x from) 1
        (= x to) 1
        (let [k (/ (- x from) h)]
        (even? k)) 2
        :else 4))


(defn simpson-integral [f from to n]
  (let [h (/ (- to from) n)]
    (defn term [x]
      (* (f (float x)) (non-y-coefficient x from to h)))
    (* (/ h 3) (sum term from #(+ % h) to))))
