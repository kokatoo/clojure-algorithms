(ns clojure-algorithms.optimization)

(defrecord SubArray [low high sum])

(defn max-idx-sum
  "Calculate the left/right index of the max sum"
  [coll offset-fn]
    (let [sums (map #(apply + %) coll)
          max-sum (apply max sums)
          idx (.indexOf sums (apply max sums))]
      [(offset-fn idx), max-sum]))

(defn max-cross-subarray [coll low mid high]
  "Max sum of subarray given mid-point"
  (let [lefts (subvec coll low (inc mid))
        rights (subvec coll (inc mid) (inc high))
        [left-idx, left-sum] (max-idx-sum (take (count lefts) (iterate rest lefts)) (partial + low))
        [right-idx, right-sum] (max-idx-sum (take (count rights) (iterate drop-last rights)) (partial - high))]
    (SubArray. left-idx right-idx (+ left-sum right-sum))))

;(take 3 (iterate rest [20 18 -23]))
;(take 3 (iterate drop-last [-23 18 20]))
;(max-cross-subarray [13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7] 3 5 8)

(defn max-subarray
  "Max SubArray using divide and conquer O(n log(n))"
  ([coll] (max-subarray coll 0 (dec (count coll))))
  ([coll low high]
   (if (= low high)
     (SubArray. low high (nth coll low))
     (let [mid (int (/ (+ low high) 2))
           left (max-subarray coll, low, mid)
           right (max-subarray coll, (inc mid), high)
           cross (max-cross-subarray coll, low, mid, high)
           max-sum (max (:sum left) (:sum right) (:sum cross))]
       (println left "," right "," cross "," max-sum)
       (cond (= max-sum (:sum left)) left
             (= max-sum (:sum right)) right
             :else cross)))))

;(= (max-subarray [13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7])
;   (SubArray. 7 10 43))
;(max-subarray [13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7]); 7 10 43
;(max-subarray [-2 -5 6 -2 -3 1 5 -6]); 2 6 7
(max-subarray [-2 4])
