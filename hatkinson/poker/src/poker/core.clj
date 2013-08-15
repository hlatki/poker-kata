(ns poker.core
  (:require [poker.helper :refer :all]))

;; Replace chars with value -- Ace is always high?  Check on this
(def value-lookup {\1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9
               \T 10 \J 11 \Q 12 \K 13 \A 14})

(def STRAIGHT_FLUSH 8)
(def FOUR_OF_A_KIND 7)
(def FULL_HOUSE 6)
(def FLUSH 5)
(def STRAIGHT 4)
(def THREE_OF_A_KIND 3)
(def TWO_PAIR 2)
(def PAIR 1)
(def HIGH_CARD 0)     

(defn parse-a-hand
  "Take a hand and return a hash containing two seqs.
  One will contain the (sorted, descending) values, the other will hold the suits.
  This assumes that the input is a string containg with the cards deliminated 
  by spaces.
  Also, it will replace the T,J,Q,K,A with a numeric value."
  [hand-string]
  (let [hand (clojure.string/split hand-string #" ")]
    {:values (into (vector) (sort > (map #(value-lookup (first %)) hand)))
    :suits (map #(second %) hand)}))


;; make this return rank and the frequency vector
;; sort values with > rather than default
(defn rank-hand
  "Naive evalator"
  [h]
  (let [hand   (parse-a-hand h)
        suits  (:suits hand)
        values (:values hand)
        freq   (frequencies values)]
    (cond
      (and (all-same-suit? suits) (consecutive? values))[:straight-flush freq]
      (four-of-a-kind? values)                          [:four-of-a-kind freq]
      (and (three-of-a-kind? values) (pair? values))    [:full-house freq]
      (all-same-suit? suits)                            [:flush freq]
      (consecutive? values)                             [:straight freq]
      (three-of-a-kind? values)                         [:three-of-a-kind freq]
      (two-pair? values)                                [:two-pair freq]
      (pair? values)                                    [:pair freq]
      :else                                             [:high-card freq])))
(def sf ["TS" "JS" "QS" "KS" "AS"])
(def f ["2S" "8S" "AS" "QS" "AS"])
(def w ["2H" "8S" "6S" "QS" "AS"])
(rank-hand sf)
(rank-hand f)
(rank-hand w)


;; compare rank -- if tie, do this to each hand frequency so that we can
;; just run compare on the two resulting vectors:
;; (map first (sort-by second > frequencies-of-sorted-hand))
