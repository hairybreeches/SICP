(ns sicp.chapter-1.ex-41)

(defn do-twice [f]
  #(f (f %)))
