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
       (cond (= max-sum (:sum left)) left
             (= max-sum (:sum right)) right
             :else cross)))))

(defn cut-rod [prices, rod-len]
  (loop [result [0] i 1 i-max nil]
    (if (> i rod-len)
      (nth result rod-len)
      (let [i-max (reduce (fn [acc j]
                            (let [j-max (+ (nth prices j) (nth result (- i j)))]
                              (if (nil? acc)
                                j-max
                                (max acc j-max))))
                          nil
                          (range 1 (inc i)))]
        (recur (conj result i-max)(inc i) i-max)))))


;(cut-rod [0 1 5 8 9 10 17 17 20 24 30] 4)
;(cut-rod [0 1 5 8 9 10 17 17 20] 8)
;(cut-rod [0 3 5 8 9 10 17 17 20] 8)
