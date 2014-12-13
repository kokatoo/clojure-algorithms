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
  (lazy-seq
   (cond (nil? left) right
         (nil? right) left
         :else (let [[l1 & *left] left
                     [r1 & *right] right]
                 (if (<= l1 r1)
                   (cons l1 (*merge *left right))
                   (cons r1 (*merge left *right)))))))

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

(defn- max-heapify [coll start end]
  (loop [coll coll x start left (inc (* 2 x))]
    (let [right (inc left)]
      (if (>= left end)
        coll
        (let [child (if (and (< left (dec end)) (< (nth coll left) (nth coll right)))
                   right
                   left)]
          (if (< (nth coll x) (nth coll child))
            (recur (swap coll x child) child (inc (* 2 child)))
            coll))))))


(defn- build-max-heap
  ([coll] (build-max-heap coll (count coll)))
  ([coll len]
    (reduce #(max-heapify %1 %2 len) (vec coll) (range (dec (int (/ len 2))) -1 -1))))

(defn heapsort [coll]
  "Heap Sort"
  (reduce #(max-heapify (swap %1 %2 0) 0 %2)(build-max-heap coll)(range (dec (count coll)) 0 -1)))

;; QuickSort
(defn quicksort [[pivot & xs :as coll]]
  "Quick Sort"
  (if pivot
    (let [smaller (filter #(< % pivot) xs)
          bigger (filter #(> % pivot) xs)
          pivots (filter #(= % pivot) coll)]
       (lazy-cat (quicksort smaller)
                 pivots
                 (quicksort bigger)))
    []))


