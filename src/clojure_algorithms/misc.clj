(ns clojure-algorithms.misc)

(defn fib-seq
  ([] (fib-seq 1 1))
  ([a b] (cons a (lazy-seq (fib-seq b (+ a b))))))

(defn fib [n]
  (last (take n (fib-seq))))

(defn factorial [n]
  (reduce * (take n (iterate inc 1))))

(defn gcd [a b]
  (if (zero? a) b
      (recur (mod b a) a)))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn dot-product [x y]
  (apply + (map * x y)))




