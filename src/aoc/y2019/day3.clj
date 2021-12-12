(ns aoc.y2019.day3
  (:require [clojure.string :as str]
            [aoc.util :as ut]
            [clojure.set :as set]))


(def raw1 "R8,U5,L5,D3")
(def raw2 "U7,R6,D4,L4")


(defn raw->edn
  [s]
  (->>(str/split (str/trim s) #",")
      (map (fn [s] [(subs s 0 1) (ut/->int (subs s 1))]))))

(defn fill
  "Takes two integers and returns the range of integers betwen them, inclusive."
  [a b]
  (if (< a b)
    (range a (inc b))
    (reverse (range b (inc a)))))

(defn plot-segment
  [[from-x from-y] [dir length]]
  (case dir
    "U" (map #(vector %1 %2)
             (repeat from-x)
             (fill (+ from-y length) from-y))
    "D" (map #(vector %1 %2)
             (repeat from-x)
             (fill (- from-y length) from-y))
    "R" (map #(vector %1 %2)
             (fill (+ from-x length) from-x)
             (repeat from-y))
    "L" (map #(vector %1 %2)
             (fill (- from-x length) from-x)
             (repeat from-y))))

(defn plot-line
  [directions]
  (loop [[to & rst] directions
         from [0 0]
         path #{}]
    (if to
      (let [[end :as line] (plot-segment from to)]
        (recur rst end (into path line)))
      path)))

(defn find-intersection
  [line1 line2]
  (->> (disj (set/intersection (plot-line line1) (plot-line line2)) [0 0])
       #_(map (partial apply +))
       #_(apply min)))

(comment
  (def ex1 (raw->edn raw1))
  (def ex2 (raw->edn raw2))

  (find-intersection ex1 ex2)
  6
  6

  (find-intersection (raw->edn "R75,D30,R83,U83,L12,D49,R71,U7,L72,U62,R66,U55,R34,D71,R55,D58,R83")
                     (raw->edn "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51,U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
  #{[69 0] [52 0] [57 0] [23 0] [1 0] [18 0] [15 0] [56 0] [71 0] [11 0] [43 0] [17 0] [68 0] [24 0] [30 0] [28 0] [3 0] [9 0] [54 0] [40 0] [59 0] [47 0] [13 0] [74 0] [29 0] [70 0] [157 4] [145 71] [8 0] [27 0] [35 0] [12 0] [16 0] [38 0] [10 0] [48 0] [72 0] [31 0] [75 0] [32 0] [21 0] [58 0] [45 0] [60 0] [157 53] [50 0] [67 0] [51 0] [20 0] [64 0] [49 0] [55 0] [62 0] [36 0] [37 0] [73 0] [66 0] [157 11] [7 0] [25 0] [41 0] [33 0] [2 0] [19 0] [61 0] [53 0] [39 0] [26 0] [5 0] [46 0] [6 0] [63 0] [65 0] [44 0] [42 0] [22 0] [34 0] [14 0] [4 0]}

  1

  (def input (map raw->edn (ut/load-input "2019/3.txt")))

  (apply find-intersection input)
  -943




  ,)
