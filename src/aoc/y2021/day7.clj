(ns aoc.y2021.day7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def ex "16,1,2,0,4,2,7,1,2,14")

(defn raw->edn
  [s]
  (map #(Integer/parseInt %) (str/split (str/trim s) #",")))

(def ex-input (raw->edn ex))

(defn cost-to-move
  [target current]
  (Math/abs (- target current)))

(defn find-alignment
  [crabs]
  (first (sort-by first (for [pos (range (count crabs))]
                          [(reduce + (map (partial cost-to-move pos) crabs)) pos]))))


(comment

  (find-alignment ex-input)
  [37 2]

  ,)

(comment
  (def input (raw->edn (slurp (io/resource "2021/day7.txt"))))
  input

  ;; part1

  (find-alignment input)
  [331067 317]

  ,)

(defn crab-move
  [target current]
  (reduce + (take (Math/abs (- target current))
                  (iterate (partial + 1) 1))))

(defn cost-for-moves
  [target crabs]
  (reduce + (map (partial crab-move target) crabs)))

(defn find-efficient-alignment
  [crabs]
  (->> (range (count crabs))
       (pmap (fn [pos] [(cost-for-moves pos crabs) pos]))
       (sort-by first)
       first))

(comment
  (find-efficient-alignment ex-input)
  [168 5]

  ;; part 2
  (time (find-efficient-alignment input))
  [92881128 458]                        ; changed `for` to `pmap`
  ;; "Elapsed time: 14836.109872 msecs"
  [92881128 458]
  ;; "Elapsed time: 37413.921255 msecs" :o
  ,)
