(ns aoc.y2021.day1
  (:require [aoc.util :as ut]))


(defn raw->edn
  [strings]
  (map #(Integer/parseInt %) strings))

(defn depth-measurement-report
  "Takes a sequence of integers representing sea floor depth."
  [depths]
  (->> depths
       ;; split them into two tuples
       (partition 2 1)
       ;; compare them
       (map (fn [[a b]] (if (< a b) :increased :decreased)))
       ;; sum them up
       (frequencies)))

(comment
  (def input (raw->edn (ut/load-input "1_1.txt")))

  ;; part1

  (depth-measurement-report input)
  ;; {:increased 1752, :decreased 247}
  )

(defn cleaned-depth-measurement-report
  "Takes a sequence of integers representing sea floor depth."
  [depths]
  (->> depths
       ;; split them into 3 tuples
       (partition 3 1)
       ;; sum the the tuples
       (map (partial apply +))
       ;; split the sums into two tuples
       (partition 2 1)
       ;; compare them
       (map (fn [[a b]] (cond (< a b) :increased
                              (> a b) :decreased
                              (= a b) :no-change)))
       ;; sum them up
       (frequencies)))

(comment
  input

  ;; part2

  (cleaned-depth-measurement-report input)
  ;; {:increased 1781, :decreased 183, :no-change 33}

  )
