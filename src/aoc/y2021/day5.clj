(ns aoc.y2021.day5
  (:require [aoc.util :as ut]
            [clojure.string :as str]))

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
        (map vector (fill x1 x2) (repeat y1))))

(defn danger-count
  [lines]
  (->> lines
       (keep line-coords)
       (mapcat identity)
       (frequencies)
       (filter (fn [[_ cnt]] (>= cnt 2)))
       (count)))

(comment
  (def input (raw->edn (ut/load-input "5_1.txt")))

  (danger-count input)

  ;; part1
  5092

  ,)

(defn all-line-coords
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

(defn accurate-danger-count
  [lines]
  (->> lines
       (keep all-line-coords)
       (mapcat identity)
       (frequencies)
       (filter (fn [[_ cnt]] (>= cnt 2)))
       (count)))

(comment
  input

  ;; part2

  (accurate-danger-count input)
  20484



  ,)
