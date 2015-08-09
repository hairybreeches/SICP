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

(def sample-text '(A D A B B C A))

(deftest can-decode
  (is (= (decode sample-message sample-tree) sample-text)))

(deftest can-encode-symbol
  (is (= (encode-symbol 'A sample-tree) '(0)))
  (is (= (encode-symbol 'B sample-tree) '(1 0)))
  (is (= (encode-symbol 'C sample-tree) '(1 1 1)))
  (is (= (encode-symbol 'D sample-tree) '(1 1 0))))

(deftest can-encode
  (is (= (encode sample-text sample-tree) sample-message)))

(deftest can-build-tree
  (is (= (generate-huffman-tree '((A 4) (C 1) (B 2) (D 1))) sample-tree)))

(def punk-symbols '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(def sha-na (cons 'SHA (repeat 8 'NA)))

(def wah-yip (cons 'WAH (repeat 9 'YIP)))

(def punk-song (concat '(GET A JOB) sha-na '(GET A JOB) sha-na wah-yip '(SHA BOOM)))

(def encoded-punk-song '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1))

(deftest can-encode-punk-songs
  (is (= (encode punk-song (generate-huffman-tree punk-symbols)) encoded-punk-song)))

(deftest song-length
  (is (= (count punk-song) 36)))

(deftest encoding-length
  (is (= (count encoded-punk-song) 84)))

(deftest can-encode-decode-punk-songs
  (let [tree (generate-huffman-tree punk-symbols)]
    (is (= (decode (encode punk-song tree) tree) punk-song))))
