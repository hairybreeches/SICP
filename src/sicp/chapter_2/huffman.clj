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
      (symbol-leaf tree)
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
