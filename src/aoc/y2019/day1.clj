(ns aoc.y2019.day1
  (:require [aoc.util :as ut]))

(defn raw->edn
  [strings]
  (map #(Integer/parseInt %) strings))

(defn calc-fuel
  [mass]
  (max (- (quot mass 3) 2) 0))


(defn calc-fuel-for-modules
  [module-masses]
  (->> module-masses
       (map calc-fuel)
       (reduce +)))

(comment
  (def input (raw->edn (ut/load-input "2019/1.txt")))

  ;; part1
  (calc-fuel-for-modules input)
  3353880

  ,)

(defn calc-all-fuel
  "Calculates fuel for mass and fuel for fuel."
  [mass]
  (->> (iterate calc-fuel (calc-fuel mass))
       (take-while pos?)
       (reduce +)))

(defn calc-fuel-for-modules-and-fuel
  [module-masses]
  (->> module-masses
       (map calc-all-fuel)
       (reduce +)))

(comment
  ;; part 2
  (calc-fuel-for-modules-and-fuel input)

  ,)
