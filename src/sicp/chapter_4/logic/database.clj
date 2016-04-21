(ns sicp.chapter-4.logic.database
  (:use sicp.chapter-4.logic.query-syntax))

(defn- indexable? [pattern]
  (or (constant-symbol? (first pattern))
      (variable? (first pattern))))

(defn- index-key-of [pattern]
  (let [k (first pattern)]
    (if (variable? k)
      '?
      k)))

(defn use-index? [pattern]
  (constant-symbol? (first pattern)))

(def rules (ref '()))

(def assertions (ref '()))

(def rule-index (ref {}))

(def assertion-index (ref {}))

(defn- get-all-assertions []
  @assertions)

(defn- get-all-rules []
  @rules)

(defn- get-all-assertions []
  @assertions)

(defn- get-indexed-assertions [pattern]
  (@assertion-index (index-key-of pattern) '()))

(defn- get-indexed-rules [pattern]
  (concat
    (@rule-index (index-key-of pattern) '())
    (@rule-index '? '())))

(defn fetch-assertions [pattern frame]
  (if (use-index? pattern)
    (get-indexed-assertions pattern)
    (get-all-assertions)))

(defn fetch-rules [pattern frame]
  (if (use-index? pattern)
    (get-indexed-rules pattern)
    (get-all-rules)))

(defn- store-in-index [index o]
  (if (indexable? o)
  (dosync
    (alter
      index
      update
      (index-key-of o)
      (fn
        [existing]
        (if existing
          (cons o existing)
          (list o)))))))



(defn- store-assertion-in-index [assertion]
  (store-in-index assertion-index assertion))

(defn- store-rule-in-index [rule]
  (store-in-index rule-index rule))

(defn- add-rule! [rule]
  (store-rule-in-index rule)
  (dosync (alter rules cons rule)))

(defn- add-assertion! [assertion]
  (store-assertion-in-index assertion)
  (dosync (alter assertions cons assertion)))

(defn add-rule-or-assertion [exp]
  (if (rule? exp)
    (add-rule! exp)
    (add-assertion! exp)))
