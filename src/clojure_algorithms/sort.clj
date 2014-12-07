(ns clojure-algorithms.sort)

(defn insertion-sort [coll]
  "insertion sort"
  (letfn [(insert [elm coll]
                  (let [[left right] (split-with (partial >= elm) coll)]
                   (reduce concat [left [elm] right])))]
    (loop [left []
           right coll]
      (if (empty? right)
        left
        (recur (insert (first right) left) (rest right))))))






