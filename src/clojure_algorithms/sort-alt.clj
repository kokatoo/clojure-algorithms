(ns clojure-algorithms.sort-alt)

(defn- swap
  "Swap elm in index i1 with elm in index i2"
  [coll i1 i2]
  (let [temp (nth coll i1)
        coll (assoc coll i1 (nth coll i2))]
    (assoc coll i2 temp)))

(defn insert
  "Insert elm in index i such that it's larger than or equal to elms 
  on the left, and smaller than elements on the right"
  [i coll]
  (if (and (pos? i) (> (nth coll (dec i)) (nth coll i)))
    (recur (dec i) (swap coll i (dec i)))
    coll))

(defn insert-sort
  "Insertion sort"
  [coll]
  (loop [coll (vec coll)
         i 1
         n (count coll)]
    (if (< i n)
      (recur (insert-swap i coll) (inc i) n)
      coll)))

(defn- h-insert
  "Generalized version of insert with gap h."
  [coll pos h]
  (if (and (>= pos h) (> (nth coll (- pos h)) (nth coll pos)))
    (recur (swap coll pos (- pos h)) (- pos h) h)
    coll))

(defn h-insert-sort
  "Sort with gap of size h"
  [coll start h]
  (loop [i (+ start h)
         coll coll
         n (count coll)]
      (if (< i n)
        (recur (+ i h) (h-insert coll i h) n)
        coll)))

(defn shell-sort
  "Shell sort"
  [coll]
  (loop [coll coll
         gap (quot (count coll) 2)]
    (if (pos? gap)
      (recur 
       (reduce #(h-insert-sort % %2 gap) coll (range gap))
       (quot gap 2))
      coll)))
