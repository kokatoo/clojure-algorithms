(ns clojure-algorithms.misc-test
  (:require [clojure.test :refer :all]
            [clojure-algorithms.misc :refer :all]))

(deftest fib-seq-test
  (testing
      (is (= '(1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040))
          (take 30 (fib-seq)))))

(deftest fib-test
  (testing
      (is (= 832040
             (fib 30)))))

