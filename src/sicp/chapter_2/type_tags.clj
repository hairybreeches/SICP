(ns sicp.chapter-2.type-tags)

(defn tag[object]
  (first object))

(defn content[object]
  (second object))

(defn attach-tag[tag content]
  [tag content])
