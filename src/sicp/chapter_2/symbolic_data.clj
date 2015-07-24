(ns sicp.chapter-2.symbolic-data)

(defn sequence-equal[a b]
  (loop [a a
         b b]
    (cond (and (empty? a) (empty? b)) true
          (empty? a) false
          (empty? b) false
          (not= (first a) (first b)) false
          :else (recur (rest a) (rest b)))))
;variables
(defn variable?[e]
  (symbol? e))

(defn same-variable?[e1 e2]
  (and (variable? e1) (variable? e2) (= e1 e2)))

;shared
(defn first-argument[e operator]
  (let [before (take-while #(not= % operator) e)]
    (if (= (count before) 1)
      (first before)
      before)))

(defn rest-arguments[e operator]
  (let [after (rest (drop-while #(not= % operator) e))]
    (if (= (count after) 1)
      (first after)
      after)))


(defn make-multi-expression[operator args]
  (interpose operator args))

;sums
(defn sum?[e]
  (and (seq? e) (some #(= % '+) e)))

(defn addend[e]
  (first-argument e '+))

(defn augend[e]
  (rest-arguments e '+))

(defn get-sum-components[sums]
  (mapcat #(list (addend %) (augend %)) sums))

(defn make-sum[& args]
  (let [number-summands (count args)
        number-split (group-by number? args)
        numbers (number-split true)
        non-numbers (number-split false)
        sum-split (group-by sum? args)
        non-sums (sum-split false)
        sums (sum-split true)]

    (cond (= number-summands 0) 0
          (= number-summands 1) (first args)
          (> (count numbers) 1) (apply make-sum (apply + numbers) non-numbers)
          (and (not (empty? numbers)) (= (first numbers) 0)) (apply make-sum non-numbers)
          (not (empty? sums)) (apply make-sum (concat (get-sum-components sums) non-sums))
          :else (mapcat #(if (seq? %) % [%]) (make-multi-expression '+ args)))))

;products
(defn product?[e]
  (and (seq? e) (not-any? #(= % '+) e) (some #(= % '*) e)))

(defn multiplier[e]
  (first-argument e '*))

(defn multiplicand[e]
  (rest-arguments e '*))

(defn get-product-components[sums]
  (mapcat #(list (multiplier %) (multiplicand %)) sums))

(defn make-product[& args]
  (let [number-multiplicands (count args)
        number-split (group-by number? args)
        numbers (number-split true)
        non-numbers (number-split false)
        product-split (group-by product? args)
        non-products (product-split false)
        products (product-split true)]
      (cond (= number-multiplicands 0) 1
          (= number-multiplicands 1) (first args)
          (> (count numbers) 1) (apply make-product (apply * numbers) non-numbers)
          (and (not (empty? numbers)) (= (first numbers) 0)) 0
          (and (not (empty? numbers)) (= (first numbers) 1)) (apply make-product non-numbers)
          (not (empty? products)) (apply make-product (concat (get-product-components products) non-products))
          :else (make-multi-expression '* args))))


(defn deriv[exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))
        (product? exp) (make-sum
                        (make-product (multiplier exp)
                                      (deriv (multiplicand exp) var))
                        (make-product (deriv (multiplier exp) var)
                                      (multiplicand exp)))

        :else (throw (Exception. (str "unknown expression type: " (apply list exp))))))

