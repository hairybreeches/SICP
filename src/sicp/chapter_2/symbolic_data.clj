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
(defn operator[e]
  (first e))

(defn first-argument[e]
  (second e))

(defn second-argument[e]
  (nth e 2))

(defn rest-arguments[e]
  (cond (= (count e) 3) (second-argument e)
        :else (list (operator e) (rest (rest e)))))

(defn make-expression[operator first-argument second-argument]
  (list operator first-argument second-argument))

;exponentiation
(defn exponentiation?[e]
  (and (seq? e) (= (operator e) '**)))

(defn base[e]
  (first-argument e))

(defn exponent[e]
  (second-argument e))

(defn make-exponentiation[base exponent]
  (cond (= exponent 0) 1
        (= exponent 1) base
        :else (make-expression '** base exponent)))
;sums
(defn sum?[e]
  (and (seq? e) (= (operator e) '+)))

(defn addend[e]
  (first-argument e))

(defn augend[e]
  (rest-arguments e))

(defn make-sum[& args]
  (let [number-split (group-by number? args)
        sum-split (group-by sum? (number-split false))
        non-sums (sum-split false)
        sums (sum-split true)
        numbers (filter #(!= % 0) (number-split true))]
    (cond (> (count numbers) 1) (apply make-sum (apply + numbers) non-numbers)
          (not (empty? sums)) (apply make-sum (apply concat
          :else (apply list '+ (concat numbers non-numbers))))

;products
(defn product?[e]
  (and (seq? e) (= (operator e) '*)))

(defn multiplier[e]
  (first-argument e))

(defn multiplicand[e]
  (rest-arguments e))

(defn make-product[e1 e2]
  (cond (or (= e1 0) (= e2 0)) 0
        (= e1 1) e2
        (= e2 1) e1
        (and (number? e1) (number? e2)) (* e1 e2)
        :else (list '* e1 e2)))


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
        (exponentiation? exp) (make-product
                                (make-product
                                  (exponent exp)
                                  (make-exponentiation (base exp) (dec (exponent exp))))
                                  (deriv (base exp) var))

        :else (throw (Exception. (str "unknown expression type: " exp)))))


