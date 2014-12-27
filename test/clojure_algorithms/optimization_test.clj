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

(deftest cut-rod-test
  (is (= (cut-rod [0 1 5 8 9 10 17 17 20 24 30] 4)
         [10 [2 2]]))
  (is (= (cut-rod [0 1 5 8 9 10 17 17 20] 8)
         [22 [2 6]]))
  (is (= (cut-rod [0 3 5 8 9 10 17 17 20] 8)
         [24 [1 1 1 1 1 1 1 1]])))



