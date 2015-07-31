(ns sicp.test.chapter-2.huffman
  (:use sicp.chapter-2.huffman)
  (:use clojure.test))

(def sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(def sample-message `(0 1 1 0 0 1 0 1 0 1 1 1 0))

(deftest can-decode
  (is (= (decode sample-message sample-tree) '(A D A B B C A))))
