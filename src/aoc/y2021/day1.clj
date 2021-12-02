(ns aoc.y2021.day1
  (:require [aoc.util :as ut]))

(def ex-depths [199
                200
                208
                210
                200
                207
                240
                269
                260
                263])

(comment
  ;; part1
  (->> ex-depths
       (partition 2 1)
       (map (fn [[a b]]
              (if (< a b)
                :increased
                :decreased))))
  ;; (:increased :increased :increased :decreased :increased :increased :increased :decreased :increased)

  ;; part2
  (->> ex-depths
       (partition 3 1)
       (map (partial apply +))
       (partition 2 1)
       (map (fn [[a b]]
              (cond
                (< a b) :increased
                (> a b) :decreased
                (= a b) :no-change))))
  ;; (:increased :no-change :decreased :increased :increased :increased :increased)

  ,)

(def input (ut/load-input "1_1.txt"))

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
  (depth-measurement-report (raw->edn input))
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
  (cleaned-depth-measurement-report (raw->edn input))
  ;; {:increased 1781, :decreased 183, :no-change 33}

  )
