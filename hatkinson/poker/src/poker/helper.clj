;; Helper functions for poker hand evaluation and comparison

(ns poker.helper)

(defn consecutive?
  "Return true if a sequence of numbers is consecutive.
  The sequence may be unsorted."
  [hand-vals]
  (second (reduce
   (fn [prev-vec curr]
     ;; prev-vec: [previous-num is-consecutive]
     ;; is-consecutive is false when it hits a nonconsecutive pairing
     (if (= (inc (first prev-vec)) curr)
       [curr (and (second prev-vec) true)]
       [curr (and (second prev-vec) false)]))
    [(dec (first hand-vals)) true]
    (into (sorted-set) hand-vals))))
