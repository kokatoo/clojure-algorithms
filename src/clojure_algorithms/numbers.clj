(ns clojure-algorithms.numbers)

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

(defn- factors [condition?]
  #(loop [factors [] n % factor 2]
     (cond
      (condition? factor %) factors
      (zero? (mod n factor)) (recur (conj factors factor) (/ n factor) factor)
      :else (recur factors n (inc factor)))))

(defn prime-factors [num]
  ((factors #(> % %2)) num))

(defn prime-seq []
  (let [_factors (factors #(> % (Math/sqrt %2)))]
    (lazy-cat [2 3 5 7 11 13]
            (filter #(empty? (_factors %))
                    (iterate inc 15)))))

(defn sieve [n]
  (loop [prime 2 seq (range 2 (inc n))]
    (if (> (* prime prime) n)
      seq
      (let [new-seq (filter #(or (= % prime) (not (zero? (mod % prime)))) seq)
            next-prime (first (filter #(> % prime) new-seq))]
        (recur next-prime new-seq)))))

(defn perfect-squares []
  (filter #(let [x (int (Math/sqrt %))]
             (= (* x x) %)) (iterate inc 1)))

