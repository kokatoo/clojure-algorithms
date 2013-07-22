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

(deftest factorial-test
  (testing
      (is (= 2432902008176640000
             (factorial 20)))))

(deftest gcd-test
  (testing
      (is (= 9
             (gcd 12345678 87654321)))))

(deftest lcm-test
  (testing
      (is (= 120239113597182
             (lcm 12345678 87654321)))))

(deftest dot-product-test
  (testing
      (is (= 156
             (dot-product (range 1 9) (range 9 0 -1))))))

(deftest convert-test
  (testing
      (is (= [1 1 1 1 1 0 0 1 1 1]
             (convert 999 2)))
    (is (= [1 7 4 7]
           (convert 999 8)))
    (is (= [3 14 7]
           (convert 999 16)))))

