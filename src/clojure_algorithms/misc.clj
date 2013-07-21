(ns clojure-algorithms.misc)

(defn fib-seq
  ([] (fib-seq 1 1))
  ([a b] (cons a (lazy-seq (fib-seq b (+ a b))))))

(defn fib [n]
  (last (take n (fib-seq))))

(defn factorial [n]
  (reduce * (take n (iterate inc 1))))




