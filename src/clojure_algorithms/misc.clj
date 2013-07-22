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

(defn convert [n base]
  (if (< n base) [n]
      (conj (convert (quot n base) base) (mod n base))))

(defn pascal-triangle [num-rows]
  (take num-rows (iterate #(concat [1] (map + % (rest %)) [1]) [1])))



