(ns clojure-algorithms.optimization-test
  (:require [clojure.test :refer :all]
            [clojure-algorithms.optimization :refer :all])
  (:import [clojure_algorithms.optimization SubArray]))

(deftest max-subarray-test
  (is (= (max-subarray [13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7])
         (SubArray. 7 10 43)))
  (is (= (max-subarray [-2 -5 6 -2 -3 1 5 -6])
         (SubArray. 2 6 7)))
  (is (= (max-subarray [-2 4])
         (SubArray. 1 1 4))))
