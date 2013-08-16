(ns poker.helper)

;; Helper functions for poker hand evaluation and comparison


;; ## Misc. functions

(defn consecutive?
  "Return true if a (sorted, descending) sequence of numbers is consecutive.
  Note that this will only work for non-negative numbers."
  [hand-vals]
  (second
    (reduce
     (fn [prev-vec curr]
       ;; prev-vec: [previous-num is-consecutive]
       ;; is-consecutive is false when it hits a nonconsecutive pairing
       (if (= (dec (first prev-vec)) curr)
         [curr (and (second prev-vec) true)]
         [curr (and (second prev-vec) false)]))
      [(inc (first hand-vals)) true]
      hand-vals)))

(defn all-same-suit?
  "Given a sequence containing the suits in a hand,
  return true if all cards in a hand are the same suit"
  [hand-suits]
  (apply = hand-suits))


;; ## Of a Kind functions
;; These functions deal with finding pairs and seeing if a hand has
;; four of a kind etc. Since the rank-hand function already calculates 
;; the frequencies for a hand, these functions all take that as their input
;; instead of recalculating it. 

(defn n-of-a-kind
  "Return frequencies matching n given the frequencies of a hand 
  (e.g. if you had a hand with values
  [5,5,6,8,4] and you were looking for pairs you would call 
  (n-of-a-kind 2 [[5 2] [6 1] [8 1] [4 1]])
  and would get [5 2] -- that is, there were two fives)"
  [n hand-freqs]
  (filter #(= (second %) n) hand-freqs))

(defn four-of-a-kind?
  "Return true if a hand has four of a kind"
  [hand-freqs]
  (not (empty? (n-of-a-kind 4 hand-freqs))))

(defn three-of-a-kind?
  "Return true if a hand has three of a kind"
  [hand-freqs]
  (not (empty? (n-of-a-kind 3 hand-freqs))))

;; ### Pairs

(defn pairs
  "Return frequencies of pairs in a hand frequency 
  (e.g. with hand [5,5,7,7,2] and hand-freq of [[5 2] [7 2] [2 1] ]you would get
  back ([5 2] [7 2])"
  [hand-freqs]
  (n-of-a-kind 2 hand-freqs))

(defn pair?
  "Return true if there is at least one pair in hand frequency"
  [hand-freqs]
  (not (empty? (pairs hand-freqs))))

(defn two-pair?
  "Return true if the hand has two pairs in hand frequency"
  [hand-freqs]
  (= (count (pairs hand-freqs)) 2))








