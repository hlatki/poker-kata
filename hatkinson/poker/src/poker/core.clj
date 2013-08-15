(ns poker.core
  (:require [poker.helper :refer :all]))

;; Replace chars with value -- Ace is always high?  Check on this
(def value-lookup {\1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9
               \T 10 \J 11 \Q 12 \K 13 \A 14})

(defn parse-a-hand
  "Take a hand and return a hash containing two seqs.
  One will contain the (sorted, descending) values, the other will hold the suits.
  This assumes that the input is a sequence of strings, like this:
  [2H 3D 5S 9C KD] (but with double quotes around each item).
  Also, it will replace the T,J,Q,K,A with a numeric value.

  The values will be sorted in ascending order, so they will be stored
  in a vector since lookup is faster from the end -- not that this matters
  when there's only five values though.
  "
  [hand]
  {:values (into (vector) (sort > (map #(value-lookup (first %)) hand)))
   :suits (map #(second %) hand)})


;; make this return rank and the frequency vector
;; sort values with > rather than default
(defn rank-hand
  "Naive evalator"
  [h]
  (let [hand   (parse-a-hand h)
        suits  (:suits hand)
        values (:values hand)]
    (cond
      (and (all-same-suit? suits) (consecutive? values)) :straight-flush
      (four-of-a-kind? values)                          :four-of-a-kind
      (and (three-of-a-kind? values) (pair? values))    :full-house
      (all-same-suit? suits)                            :flush
      (consecutive? values)                             :straight
      (three-of-a-kind? values)                         :three-of-a-kind
      (two-pair? values)                                :two-pair
      (pair? values)                                    :pair
      :else                                             :high-card)))
(def sf ["TS" "JS" "QS" "KS" "AS"])
(def f ["2S" "8S" "AS" "QS" "AS"])
(def w ["2H" "8S" "6S" "QS" "AS"])
(rank-hand sf)
(rank-hand f)
(rank-hand w)


;; compare rank -- if tie, do this to each hand frequency so that we can
;; just run compare on the two resulting vectors:
;; (map first (sort-by second > frequencies-of-sorted-hand))
