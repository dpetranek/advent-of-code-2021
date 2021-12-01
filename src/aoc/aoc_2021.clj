(ns aoc.aoc-2021
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(comment
  (def depths [199
               200
               208
               210
               200
               207
               240
               269
               260
               263])

  (->> depths
       (partition 2 1)
       (map (fn [[a b]]
              (if (< a b)
                :increased
                :decreased))))
  (:increased :increased :increased :decreased :increased :increased :increased :decreased :increased)

  (->> depths
       (partition 3 1)
       (map (partial apply +))
       (partition 2 1)
       (map (fn [[a b]]
              (cond
                (< a b) :increased
                (> a b) :decreased
                (= a b) :no-change
                :else (throw (Exception. "bad"))))))
  (:increased :no-change :decreased :increased :increased :increased :increased)

  (607 618 618 617 647 716 769 792)
  ((199 200 208) (200 208 210) (208 210 200) (210 200 207) (200 207 240) (207 240 269) (240 269 260) (269 260 263))





  (slurp (io/resource "test.txt"))


  (->> (slurp (io/resource "1_1.txt"))
       (str/split-lines)
       (map #(Integer/parseInt %))
       (partition 2 1)
       (map (fn [[a b]]
              (if (< a b)
                :increased
                :decreased)))

       (frequencies))
  {:increased 1752, :decreased 247}

  (->> (slurp (io/resource "1_1.txt"))
       (str/split-lines)
       (map #(Integer/parseInt %))
       (partition 3 1)
       (map (partial apply +))
       (partition 2 1)
       (map (fn [[a b]]
              (cond
                (< a b) :increased
                (> a b) :decreased
                (= a b) :no-change
                :else (throw (Exception. "bad")))))
       (frequencies))
  {:increased 1781, :decreased 183, :no-change 33}





  ,,,)
