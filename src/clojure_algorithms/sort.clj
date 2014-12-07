(ns clojure-algorithms.sort)

(defn insert-sort [coll]
  "insertion sort"
  (letfn [(insert [elm coll]
                  (into []
                   (concat (filter #(<= % elm) coll) [elm] (filter #(> % elm) coll))))]
    (loop [left []
           right coll]
      (if (empty? right)
        left
        (recur (insert (first right) left) (rest right))))))






