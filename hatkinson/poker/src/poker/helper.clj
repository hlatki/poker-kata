;; Helper functions for poker hand evaluation and comparison

(ns poker.helper)

(defn consecutive?
  "Return true if a (sorted, ascending) sequence of numbers is consecutive.
  Note that this will only work for positive numbers (since there are no positive
  numbers in poker)."
  [hand-vals]
  (second (reduce
   (fn [prev-vec curr]
     ;; prev-vec: [previous-num is-consecutive]
     ;; is-consecutive is false when it hits a nonconsecutive pairing
     (if (= (inc (first prev-vec)) curr)
       [curr (and (second prev-vec) true)]
       [curr (and (second prev-vec) false)]))
    [(dec (first hand-vals)) true]
    hand-vals)))


(defn n-of-a-kind
  "Return frequencies matching n (e.g. if you had a hand with values
  [5,5,6,8,4] and you were looking for pairs you would call (n-of-a-kind 2 [5,5,6,8,4])
  and would get [5 2] -- that is, there were two fives)"
  [n hand-values]
  (filter #(= (second %) n)
          (frequencies hand-values)))

