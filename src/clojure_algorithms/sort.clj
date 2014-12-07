(ns clojure-algorithms.sort)

(defn insert-sort [coll]
  "insertion sort"
  (letfn [(insert [elm coll]
                  (let [[left right] (split-with (partial >= elm) coll)]
                   (doall (reduce concat [left [elm] right]))))]
    (loop [left []
           right coll]
      (if (empty? right)
        left
        (recur (insert (first right) left) (rest right))))))






