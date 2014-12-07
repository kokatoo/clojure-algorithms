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
         
(defn *merge [left right]
  (cond (nil? left) right
        (nil? right) left
        :else (let [[l1 & *left] left
                    [r1 & *right] right]
                (if (<= l1 r1)
                  (cons l1 (*merge *left right))
                  (cons r1 (*merge left *right))))))
                
(defn merge-sort [coll]
  (let [[elm & *coll] coll]
    (if (nil? *coll)
      coll
      (let [[left right] (split-at (/ (count coll) 2) coll)]
        (*merge (merge-sort left) (merge-sort right))))))




