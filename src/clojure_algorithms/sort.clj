(ns clojure-algorithms.sort)

(defn insert-sort [coll]
  "Insertion Sort"
  (letfn [(insert [elm coll]
                  (into []
                   (concat (filter #(<= % elm) coll) [elm] (filter #(> % elm) coll))))]
    (loop [left []
           right coll]
      (if (empty? right)
        left
        (recur (insert (first right) left) (rest right))))))

(defn *merge [left right]
  "Merge two sorted collections into sorted collection"
  (cond (nil? left) right
        (nil? right) left
        :else (let [[l1 & *left] left
                    [r1 & *right] right]
                (if (<= l1 r1)
                  (cons l1 (*merge *left right))
                  (cons r1 (*merge left *right))))))

(defn *merge-iter [left right]
  "Merge two sorted collections into sorted collection (iteration)"
  (letfn [(**merge [left right result]
                   (let [[l1 & *left] left
                         [r1 & *right] right]
                     (cond
                      (and (nil? l1) (nil? r1)) result
                      (and (some? l1) (some? r1))
                        (if (<= l1 r1)
                          (recur *left right (conj result l1))
                          (recur left *right (conj result r1)))
                      (nil? l1)
                        (recur left *right (conj result r1))
                      (nil? r1)
                        (recur *left right (conj result l1)))))]
    (**merge left right [])))

(defn merge-sort
  "Merge Sort"
  ([coll] (merge-sort coll false))
  ([coll iterate?]
   (let [[elm & *coll] coll]
    (if (nil? *coll)
      coll
      (let [[left right] (split-at (/ (count coll) 2) coll)]
        (if iterate?
          (*merge (merge-sort left) (merge-sort right))
          (*merge-iter (trampoline #(merge-sort left true))
                       (trampoline #(merge-sort right true)))))))))
