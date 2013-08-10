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