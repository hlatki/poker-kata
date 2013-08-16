(ns poker.core
  (:require [poker.helper :refer :all]))

;;
;; These functions are the core poker hand comparison and ranking functions. 
;; If you want to comapre two hands, call (compare-hands white black), where
;; white and black are each strings representing a particular hands.
;;
;; In each hand, each card is represented by two characters: the first is the
;; value of the card, and the second is its suit. The cards are deliminated by
;; spaces.  
;;



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
  "Take a hand and return a vector containing two seqs.
  One will contain the (sorted, descending) values, the other will hold the suits.
  This assumes that the input is a string containing the cards (deliminated
  by spaces).
  Also, it will replace the T,J,Q,K,A with a numeric value."
  [hand-string]
  (let [hand (clojure.string/split hand-string #" ")]
    {:values (into (vector) (sort > (map #(value-lookup (first %)) hand)))
    :suits (map #(second %) hand)}))



(defn rank-hand
  "Naive evaluator. Returns a vector containing the rank and the frequency of
  the values"
  [h]
  (let [hand   (parse-a-hand h)
        suits  (:suits hand)
        values (:values hand)
        freq   (frequencies values)]
    (cond
      (and (all-same-suit? suits) (consecutive? values))[STRAIGHT_FLUSH freq]
      (four-of-a-kind? freq)                            [FOUR_OF_A_KIND freq]
      (and (three-of-a-kind? freq) (pair? freq))        [FULL_HOUSE freq]
      (all-same-suit? suits)                            [FLUSH freq]
      (consecutive? values)                             [STRAIGHT freq]
      (three-of-a-kind? freq)                           [THREE_OF_A_KIND freq]
      (two-pair? freq)                                  [TWO_PAIR freq]
      (pair? freq)                                      [PAIR freq]
      :else                                             [HIGH_CARD freq])))

(defn tie-breaker-freq
  "Used to break ties. Sorts the value by number of repititions. i.e., if you
  had 3 fives and 2 eights, the output would be [5 8]. Since both hands are of the
  same rank we can just compare the two vectors to break ties."
  [hand-freq]
  (map first (sort-by second > hand-freq)))



(defn compare-hands
  "Compare two hands and return the winner. This is the only function 
  you would need to call in order to compare two hands. It takes two hands,
  each as a string."
  [white-hand black-hand]
  (let [white   (rank-hand white-hand)
        black   (rank-hand black-hand)
        winner  (compare (first white) (first black))
        decider {-1 "Black wins" 0 "a tie?!" 1 "White wins"}]
    (if (= winner 0)
      (decider (compare
                (into (vector)
                  (tie-breaker-freq (second white)))
                (into (vector)
                  (tie-breaker-freq (second black)))))
      (decider winner))))



