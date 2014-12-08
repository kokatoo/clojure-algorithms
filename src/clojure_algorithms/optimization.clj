(ns clojure-algorithms.optimization)

(defn max-idx-sum [coll offset]
  (if (empty? coll) nil
    (let [sums (map #(apply + %) coll)
          max-sum (apply max sums)
          idx (.indexOf sums (apply max sums))]
      [(+ idx offset), max-sum])))

(defn max-cross-subarray [coll low mid high]
  (let [lefts (subvec coll low (inc mid))
        rights (subvec coll (inc mid) (inc high))
        [left-idx, left-sum] (max-idx-sum (take (count lefts) (iterate rest lefts)) low)
        [right-idx, right-sum] (max-idx-sum (reverse (take (count rights) (iterate drop-last rights))) (inc mid))]
    [left-idx, right-idx, (+ left-sum right-sum)]))

;(take 3 (iterate rest [20 18 -23]))
;(take 3 (iterate drop-last [-23 18 20]))
;(max-cross-subarray [13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7] 3 5 8)

(defn max-subarray
  ([coll] (max-subarray coll 0 (dec (count coll))))
  ([coll low high]
   (if (= low high)
     [low, high, (nth coll low)]
     (let [mid (int (/ (+ low high) 2))
           [left-low, left-high, left-sum] (max-subarray coll, low, mid)
           [right-low, right-high, right-sum] (max-subarray coll, (inc mid), high)
           [cross-low, cross-high, cross-sum] (max-cross-subarray coll, low, mid, high)
           max-sum (max left-sum right-sum)]
       (cond (= max-sum left-sum) [left-low, left-high, left-sum]
             (= max-sum right-sum) [right-low, right-high, right-sum]
             :else [cross-low, cross-high, cross-sum])))))


(max-subarray [13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7])
