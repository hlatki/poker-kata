(ns poker.helper-test
  (:require [clojure.test :refer :all]
            [poker.helper :refer :all]))

;; ## Tests for consecutive? function
(deftest sorted-consecutive
  (testing "Testing sorted consecutive sequences")
  (consecutive? [1 2 3 4 5 6])
  (consecutive? [0 2 3 4 5 6])
  (consecutive? #{1 2 3 4 5 6})
  (consecutive? '(1 2 3 4 5 6)))
(deftest unsorted-consecutive
  (testing "Testing unsorted sequences that are consecutive")
  (consecutive? (into (sorted-set) [1 4 2 3 5]))
  (consecutive? (into (sorted-set) [1 3 2 0 4])))
(deftest unsorted-nonconsecutive
  (testing "Testing unsorted sequences that are not consecutive")
  (not (consecutive? [5 7 3 4 1]))
  (not (consecutive? [-5 -2 -3 -4 -9])))
