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

(defn cut-rod-aux [prices, rod-len]
  (loop [results [0] paths [0] i 1]
    (if (> i rod-len)
      [results paths]
      (let [[i-max prev] (reduce (fn [acc j]
                                    (let [j-max (+ (nth prices j) (nth results (- i j)))]
                                      (if (or (nil? acc) (> j-max (first acc)))
                                        [j-max j]
                                        acc)))
                                  nil
                                  (range 1 (inc i)))]
        (recur (conj results i-max) (conj paths prev) (inc i))))))

(defn cut-rod [prices rod-len]
  (let [[results paths] (cut-rod-aux prices rod-len)]
    (loop [idx rod-len path []]
      (if (> idx 0)
        (recur (- idx (nth paths idx)) (conj path (nth paths idx)))
        [(nth results rod-len) path]))))


;(cut-rod [0 1 5 8 9 10 17 17 20 24 30] 4)
;(cut-rod [0 1 5 8 9 10 17 17 20] 8)
;(cut-rod [0 3 5 8 9 10 17 17 20] 8)
;(cut-rod [0 1 5 8 9 10 17 17 20 24 30] 4)

;; WIP
(defn m-c3 [p m i j]
  (loop [k i m (assoc-in m [i j] nil)]
    (println m)
    (if (> k (dec j))
      m
      (let [q (+ (get-in m [i k])
                 (get-in m [(inc k) j])
                 (* (p (dec i))
                    (p k)
                    (p j)))]
        (println m ", k: " k ", i: " i ", j: " j)
        (println "q2: " q)
        (if (or (nil? (get-in m [i j])) (< q (get-in m [i j])))
          (recur (inc k)(assoc-in m [i j] q))
          (recur (inc k) m))))))

(defn m-c2 [p m l n]
  (loop [m m i 1]
    (if (> i (- n l -1))
      m
      (let [m (m-c3 p m i (+ i l -1))]
        (recur m (inc i))))))

(defn matrix-chain [p n]
  (loop [l 2 m (->> 0 (replicate (inc n)) vec (replicate (inc n)) vec)]
    (if (> l n)
      m
      (let [m (m-c2 p m l n)]
        (recur (inc l) m)))))

(matrix-chain [40 20 30 10 30] 4)



















