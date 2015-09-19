(ns sicp.chapter-2.huffman)
;creating
(defn make-leaf[sym weight]
  (list 'leaf sym weight))

(defn leaf? [object]
  (= (first object) 'leaf))

(defn symbol-leaf[leaf]
  (second leaf))

(defn weight-leaf[leaf]
  (nth leaf 2))

(defn left-branch[tree]
  (first tree))

(defn right-branch[tree]
  (second tree))

(defn symbols[tree]
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (nth tree 2)))

(defn weight[tree]
  (if (leaf? tree)
      (weight-leaf tree)
      (nth tree 3)))

(defn make-code-tree [left right]
  (list
   left
   right
   (concat (symbols left) (symbols right))
   (+ (weight left) (weight right))))

(defn adjoin-set[x a-set]
  (cond (empty? a-set) (list x)
        (< (weight x) (weight (first a-set))) (cons x a-set)
        :else (cons (first a-set) (adjoin-set x (rest a-set)))))

(defn make-leaf-set[pairs]
  (if (empty? pairs)
      '()
      (let [pair (first pairs)]
        (adjoin-set (make-leaf (first pair) (second pair))
                    (make-leaf-set (rest pairs))))))

(defn successive-merge[leaf-set]
  (loop[node-set leaf-set]
    (if (= (count node-set) 1)
      (first node-set)
      (let [new-node (make-code-tree (first node-set) (second node-set))]
        (recur (adjoin-set new-node (rest (rest node-set))))))))


(defn generate-huffman-tree[pairs]
  (successive-merge (make-leaf-set pairs)))


;decoding
(defn choose-branch[bit branch]
  (cond
     (= bit 0) (left-branch branch)
     (= bit 1) (right-branch branch)
     :else (throw (Exception. "bad bit in choose-branch"))))


(defn decode [bits tree]
  (reverse (loop [bits bits
         current-branch tree
         message '()]
    (if (empty? bits)
        message
        (let [next-branch (choose-branch (first bits) current-branch)]
          (if (leaf? next-branch)
            (recur (rest bits) tree (cons (symbol-leaf next-branch) message))
            (recur (rest bits) next-branch message)))))))

;encoding
(defn branch-contains?[sym branch]
  (some #(= sym %) (symbols branch)))

(defn encode-symbol [sym tree]
  (reverse
    (loop [current-branch tree
           result '()]
      (if (not (branch-contains? sym current-branch))
          (throw (Exception. "symbol not in branch!"))
          (cond
            (leaf? current-branch) result
            (branch-contains? sym (left-branch current-branch)) (recur (left-branch current-branch) (cons 0 result))
            :else (recur (right-branch current-branch) (cons 1 result)))))))

(defn encode[message tree]
  (if (empty? message)
    '()
    (concat
      (encode-symbol (first message) tree)
      (encode (rest message) tree))))

