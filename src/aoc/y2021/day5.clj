(ns aoc.y2021.day5
  (:require [aoc.util :as ut]
            [clojure.string :as str]))

(def ex-lines
  ["0,9 -> 5,9"
   "8,0 -> 0,8"
   "9,4 -> 3,4"
   "2,2 -> 2,1"
   "7,0 -> 7,4"
   "6,4 -> 2,0"
   "0,9 -> 2,9"
   "3,4 -> 1,4"
   "0,0 -> 8,8"
   "5,5 -> 8,2"])

(defn raw->edn
  [strings]
  (map (fn [s] (->> (str/split s #" -> ")
                    (map #(str/split % #","))
                    (map (partial map #(Integer/parseInt %)))))
       strings))

(defn fill
  "Takes two integers and returns the range of integers betwen them, inclusive."
  [a b]
  (if (< a b)
    (range a (inc b))
    (reverse (range b (inc a)))))

(defn line-coords
  [[[x1 y1] [x2 y2]]]
  (cond (= x1 x2)
        ;; vertical
        (map vector (repeat x1) (fill y1 y2))

        (= y1 y2)
        ;; horizontal
        (map vector (fill x1 x2) (repeat y1))

        :else
        ;; diagonal
        (map vector (fill x1 x2) (fill y1 y2))))

(defn danger-count
  [lines]
  (->> lines
       (keep line-coords)
       (mapcat identity)
       (frequencies)
       (filter (fn [[coord cnt]] (>= cnt 2)))
       (count)))

(comment

  (def ex (raw->edn ex-lines))

  (map vector (fill 1 3) (fill 1 3))
  ([1 1] [2 2] [3 3])
  ([1 1] [2 2] [3 3])

  (map vector (fill 9 7) (fill 7 9))
  ([9 7] [8 8] [7 9])
  ([7 7] [8 8] [9 9])

  (map vector
       (repeatedly 4)
       (range 4))
  (->> ex
       (keep line-coords)
       (mapcat identity)
       (frequencies)
       (filter (fn [[coord cnt]] (>= cnt 2)))
       (count))
  12
  15
  5
  ([[7 4] 2] [[3 4] 2] [[1 9] 2] [[2 9] 2] [[0 9] 2])

  ex
  (((0 9) (5 9))
   ((8 0) (0 8))
   ((9 4) (3 4))
   ((2 2) (2 1))
   ((7 0) (7 4))
   ((6 4) (2 0))
   ((0 9) (2 9))
   ((3 4) (1 4))
   ((0 0) (8 8))
   ((5 5) (8 2)))

  (map line-coords ex)
  (([0 9] [1 9] [2 9] [3 9] [4 9] [5 9])
   nil
   ([3 4] [4 4] [5 4] [6 4] [7 4] [8 4] [9 4])
   ([2 1] [2 2])
   ([7 0] [7 1] [7 2] [7 3] [7 4])
   nil
   ([0 9] [1 9] [2 9])
   ([1 4] [2 4] [3 4])
   nil
   nil)

  (([9 0] [9 1] [9 2] [9 3] [9 4] [9 5])
   nil
   ([4 3] [4 4] [4 5] [4 6] [4 7] [4 8] [4 9])
   ([2 1] [2 2])
   ([7 0] [7 1] [7 2] [7 3] [7 4])
   nil
   ([9 0] [9 1] [9 2])
   ([4 1] [4 2] [4 3])
   nil
   nil)

  ([[4 3] 2]
   [[9 0] 2]
   [[9 2] 2]
   [[9 1] 2])


  (map line-coords ex)

  (:horizontal [[8 0] [0 8]] :horizontal :vertical :vertical [[6 4] [2 0]] :horizontal :horizontal [[0 0] [8 8]] [[5 5] [8 2]])
  (:horizontal nil :horizontal :vertical :vertical nil :horizontal :horizontal nil nil)

  (fill 4 8)

  (seq (range 8 8))

  (seq (range 8 4))
  nil
  (seq (range 4 8))
  (4 5 6 7)



  '(((0 9) (5 9))
    ((8 0) (0 8))
    ((9 4) (3 4))
    ((2 2) (2 1))
    ((7 0) (7 4))
    ((6 4) (2 0))
    ((0 9) (2 9))
    ((3 4) (1 4))
    ((0 0) (8 8))
    ((5 5) (8 2)))



  (def input (raw->edn (ut/load-input "5_1.txt")))

  (danger-count input)
  ;; part2
  20484

  ;; part1
  5092

  ,)
