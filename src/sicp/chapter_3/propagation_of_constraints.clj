(ns sicp.chapter-3.propagation_of_constraints
  (:use clojure.math.numeric-tower))

(defn has-value?
  [connector]
  (connector :has-value?))

(defn get-value
  [connector]
  (connector :value))

(defn set-value!
  [connector new-value informant]
  ((connector :set-value!) new-value informant))

(defn forget-value!
  [connector retractor]
  ((connector :forget) retractor))

(defn connect
  [connector new-constraint]
  ((connector :connect) new-constraint))

(defn inform-about-value
  [constraint]
  (constraint :i-have-a-value))

(defn inform-about-no-value
  [constraint]
  (constraint :i-lost-my-value))

(defn for-each-except
  [exception procedure things]
  (loop
    [things things]
    (if (empty? things)
        :done
        (do (if (not (= (first things) exception))
                (procedure (first things)))
            (recur (rest things))))))

(defn adder
  [a1 a2 sum]
  (letfn
    [(me
      [request]
       (cond
        (= request :i-have-a-value) (process-new-value)
        (= request :i-lost-my-value) (process-forget-value)))

     (process-new-value
     []
       (cond (and (has-value? a1) (has-value? a2))
             (set-value! sum
                         (+ (get-value a1) (get-value a2))
                         me)
             (and (has-value? a1) (has-value? sum))
             (set-value! a2
                         (- (get-value sum) (get-value a1))
                         me)
             (and (has-value? a2) (has-value? sum))
             (set-value! a1
                         (- (get-value sum) (get-value a2))
                         me)))

     (process-forget-value
     []
       (forget-value! sum me)
       (forget-value! a1 me)
       (forget-value! a2 me))]


      (connect a1 me)
      (connect a2 me)
      (connect sum me)
      me))

(defn multiplier
  [m1 m2 product]

  (letfn
        [(me
         [request]
         (cond
          (= request :i-have-a-value) (process-new-value)
          (= request :i-lost-my-value) (process-forget-value)))

        (process-new-value
        []
          (cond
           (or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me)

           (and (has-value? m1)
                (has-value? m2))
           (set-value! product (* (get-value m1) (get-value m2)) me)

           (and (has-value? m1)
                (has-value? product))
           (set-value! m2 (/ (get-value product) (get-value m1)) me)

           (and (has-value? m2)
                (has-value? product))
           (set-value! m1 (/ (get-value product) (get-value m2)) me)))

        (process-forget-value
        []
          (forget-value! product me)
          (forget-value! m1 me)
          (forget-value! m2 me))]

      (connect m1 me)
      (connect m2 me)
      (connect product me)
      me))

(defn constant
  [value connector]
  (let
    [me (fn
          [request]
          (throw (Exception. "Can't update a constant")))]

    (connect connector me)
    (set-value! connector value me)
    me))

(defn make-connector
  []
  (let
    [value (ref false)
     informant (ref false)
     constraints (ref #{})]
    (letfn[

     (set-my-value
       [newval setter]
       (cond (not (has-value? me))
             (dosync
              (ref-set value newval)
              (ref-set informant setter)
              (for-each-except setter inform-about-value @constraints))

             (not (= @value newval))
             (throw (Exception. (str "Contradiction: " @value " " newval)))

             :else
             :ignored))

     (forget-my-value
     [retractor]
       (if (= retractor @informant)
         (dosync
          (ref-set informant false)
          (for-each-except retractor inform-about-no-value @constraints))))

     (connect
       [new-constraint]
       (if (not (@constraints new-constraint))
           (dosync
            (alter constraints conj new-constraint)))

       (if (has-value? me)
         (inform-about-value new-constraint)))

    (me
     [request]
       (cond
        (= request :has-value?) (not (not @informant))
        (= request :value) @value
        (= request :set-value!) set-my-value
        (= request :forget) forget-my-value
        (= request :connect) connect))]
     me)))

(defn c+
  [a1 a2]
  (let [sum (make-connector)]
    (adder a1 a2 sum)
    sum))

(defn c*
  [m1 m2]
  (let [product (make-connector)]
    (multiplier m1 m2 product)
    product))

(defn c-
  [sum a1]
  (let [a2 (make-connector)]
    (adder a1 a2 sum)
    a2))

(defn c-div
  [product m1]
  (let [m2 (make-connector)]
    (multiplier m1 m2 product)
    m2))

(defn cv
  [value]
  (let [conn (make-connector)]
    (constant value conn)
    conn))

(defn celsius-to-fahrenheit-converter
  [c]
  (c+ (c-div (c* c (cv 9)) (cv 5)) (cv 32)))

(defn averager
  [a b]
  (c-div (c+ a b) (cv 2)))

(defn squarer
  [root square]
  (letfn [
        (me
         [request]
         (cond
          (= request :i-have-a-value) (process-new-value)
          (= request :i-lost-my-value) (process-forget-value)))

        (process-new-value
        []
          (if (has-value? square)
            (if (< (get-value square) 0)
                (throw (Exception. (str "Square less than 0: " (get-value square))))
                (set-value! root (sqrt (get-value square)) me))
            (set-value! square (* (get-value root) (get-value root)) me)))

        (process-forget-value
          []
            (forget-value! root me)
            (forget-value! square me))]

      (connect root me)
      (connect square me)
      me))

























