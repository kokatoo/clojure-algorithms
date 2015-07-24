(ns clojure-algorithms.sort)

(defn- insert
  "left of elm are <= and right of elm are >"
  [elm coll]
  (into [] (concat (filter #(<= % elm) coll), [elm], (filter #(> % elm) coll))))

(defn insert-sort
  "Insertion sort"
  [coll]
  (loop [left []
         right coll]
    (if (empty? right)
      left
      (recur (insert (first right) left), (rest right)))))

(defn h-insert-sort
  "Generalized version of insert with gap h."
  [coll start h]
  (let [hcoll (partition h coll)
        first-hcoll (map first hcoll)
        rest-hcoll (map rest hcoll)]
    (->> rest-hcoll
         (interleave (insert-sort first-hcoll))
         flatten)))

(defn shell-sort
  [coll]
  (let [gaps (rest (take-while pos? (iterate #(quot % 2) (count coll))))]
    (reduce (fn [coll gap] ; h-sort for each gap
              (reduce #(h-insert-sort % %2 gap) coll (range gap))) ; for each gap, h-sort for first h indexes
            coll  
            gaps)))


(defn *merge
  "Merge two sorted collections into sorted collection"
  [left right]
  (lazy-seq
   (cond (nil? left) right
         (nil? right) left
         :else (let [[l1 & *left] left
                     [r1 & *right] right]
                 (if (<= l1 r1)
                   (cons l1 (*merge *left right))
                   (cons r1 (*merge left *right)))))))

(defn *merge-iter
  "Merge two sorted collections into sorted collection (iteration)"
  [left right]
  (letfn [(**merge
           [left right result]
           (let [[l1 & *left] left
                 [r1 & *right] right]
             (cond
              (and (nil? l1) (nil? r1)) result
              (and (some? l1) (some? r1)) (if (<= l1 r1)
                                            (recur *left right (conj result l1))
                                            (recur left *right (conj result r1)))
              (nil? l1) (recur left *right (conj result r1))
              (nil? r1) (recur *left right (conj result l1)))))]
    (**merge left right [])))

(defn merge-sort-wrapper
  "Wrap common splitting code"
  [coll *merge-sort]
  (let [[elm & *coll] coll]
    (if (nil? *coll)
      coll
      (let [[left right] (split-at (/ (count coll) 2) coll)]
        (*merge-sort left right)))))

(defn merge-sort
  "Merge Sort (Recursive)"
  [coll]
  (merge-sort-wrapper
   coll
   (fn [left right]
     (*merge (merge-sort left) (merge-sort right)))))

(defn merge-sort-iter
  "Merge Sort (Iterative)"
  [coll]
  (merge-sort-wrapper
   coll
   (fn [left right]
     (*merge-iter (trampoline #(merge-sort-iter left))
                  (trampoline #(merge-sort-iter right))))))

;; HeapSort
(defn- swap [coll i j]
  (assoc coll i (nth coll j) j (nth coll i)))
(defn- in? [elm coll]
  (some #(= elm %) coll))

(defn- max-heapify
  [coll start end]
  (loop [coll coll x start left (inc (* 2 x))]
    (let [right (inc left)]
      (if (>= left end)
        coll
        (let [child
              (if (and (< left (dec end)) (< (nth coll left) (nth coll right)))
                right
                left)]
          (if (< (nth coll x) (nth coll child))
            (recur (swap coll x child) child (inc (* 2 child)))
            coll))))))

(defn- build-max-heap
  ([coll] (build-max-heap coll (count coll)))
  ([coll len]
    (reduce #(max-heapify %1 %2 len)
            (vec coll)
            (range (dec (int (/ len 2))) -1 -1))))

(defn heapsort
  "Heap Sort"
  [coll]
  (reduce #(max-heapify (swap %1 %2 0) 0 %2)
          (build-max-heap coll)
          (range (dec (count coll)) 0 -1)))

;; Quickort
(defn quicksort
  "Quick Sort"
  [[pivot & xs :as coll]]
  (if pivot
    (let [smaller (filter #(< % pivot) xs)
          bigger (filter #(> % pivot) xs)
          pivots (filter #(= % pivot) coll)]
       (lazy-cat (quicksort smaller)
                 pivots
                 (quicksort bigger)))
    []))

(defn rand-quicksort
  "Rand Pivot QuickSort"
  [coll]
  (if (empty? coll)
    []
    (let [pivot (nth coll (rand-int (count coll)))
          xs (remove #(= % pivot) coll)
          smaller (filter #(< % pivot) xs)
          bigger (filter #(> % pivot) xs)
          pivots (filter #(= % pivot) coll)]
      (lazy-cat (rand-quicksort smaller)
                pivots
                (rand-quicksort bigger)))))

;; Counting Sort
(defn- counts-less-eq
  "Returns a vector of counts <= to index"
  [coll n-buckets *get-elm]
  (let [len
        (count coll)
        counts-eq
        (reduce (fn [acc idx]
                  (let [elm (*get-elm coll idx)]
                    (assoc acc elm (inc (acc elm)))))
                (-> (replicate n-buckets 0) vec)
                (range len))]
    (reduce (fn [acc idx]
              (assoc acc idx (+ (acc idx) (acc (dec idx)))))
            counts-eq
            (range 1 n-buckets))))

(defn countingsort
  "Counting Sort"
  ([coll n-buckets]
   (countingsort
    coll
    n-buckets
    (fn [coll idx]
      (nth coll idx))))
  ([coll n-buckets *get-elm]
   (let [len (count coll)
         coll (vec coll)]
     (loop [result (vec (replicate len 0))
            counts (counts-less-eq coll n-buckets *get-elm)
            idx (dec len)]
       (if (= idx -1)
         (rest result)
         (recur (assoc result (counts (*get-elm coll idx)) (nth coll idx))
                (assoc counts (*get-elm coll idx) (dec (counts (*get-elm coll idx))))
                (dec idx)))))))

;; Radix Sort
(defn- ret-pos
  "Returns the digit by pos"
  [num base pos]
  (let [exp (Math/pow base pos)]
    (-> (/ num exp)
        (mod base)
        int)))

(defn radixsort
  "Radix Sort"
  [coll ndigits]
  (let [len (count coll)
        coll (vec coll)]
    (loop [i 0 result coll]
      (if (= i ndigits)
        result
        (recur (inc i)
               (countingsort
                result
                10
                (fn [coll idx]
                  (ret-pos (nth coll idx) 10 i))))))))
