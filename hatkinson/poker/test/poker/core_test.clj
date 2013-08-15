(ns poker.core-test
  (:require [clojure.test :refer :all]
            [poker.core :refer :all]))

(defn get-rank
  "helper function to get rank of hand, given hand as string"
  [hand-string]
  (first (rank-hand (clojure.string/split hand-string #" "))))


(deftest a-test
  (testing "Test that hands are classified correctly"
    (is (get-rank "TS JS QS KS AS") :straight-flush)
    (is (get-rank "2D 2S 2H 2C KH") :four-of-a-kind)
    (is (get-rank "2D 2H 2S AH AS") :full-house)
    (is (get-rank "AH TH 8H 6H JH") :flush)
    (is (get-rank "6H 7S 8H 9S TS") :straight)
    (is (get-rank "2D 2S 2H JC AH") :three-of-a-kind)
    (is (get-rank "3H AC 3D AS KD") :two-pair)
    (is (get-rank "AH KS AC TS 2H") :pair)
    (is (get-rank "6H 2C QH 3S TH") :high-card)))
